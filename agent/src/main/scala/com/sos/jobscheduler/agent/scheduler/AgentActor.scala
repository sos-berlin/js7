package com.sos.jobscheduler.agent.scheduler

import akka.Done
import akka.actor.{ActorRef, Props, Status, Terminated}
import akka.pattern.ask
import akka.util.Timeout
import com.sos.jobscheduler.agent.data.commandresponses.EmptyResponse
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.scheduler.AgentActor._
import com.sos.jobscheduler.agent.scheduler.job.task.TaskRunner
import com.sos.jobscheduler.agent.scheduler.job.{JobKeeper, JobRunner}
import com.sos.jobscheduler.agent.scheduler.order.AgentOrderKeeper
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.akkautils.Akkas
import com.sos.jobscheduler.common.akkautils.Akkas.StoppingStrategies
import com.sos.jobscheduler.common.auth.UserId
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.data.jobnet.JobPath
import com.sos.jobscheduler.shared.common.ActorRegister
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.{GzipCompression, JsonJournalActor, JsonJournalMeta, JsonJournalRecoverer, KeyedEventJournalingActor}
import java.nio.file.Path
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future, Promise}

/**
  * @author Joacim Zschimmer
  */
private[scheduler] final class AgentActor(
  jobConfigurationDirectory: Path,
  stateDirectory: Path,
  implicit private val askTimeout: Timeout,
  syncOnCommit: Boolean,
  stoppedPromise: Promise[Completed])
  (implicit
    timerService: TimerService,
    keyedEventBus: StampedKeyedEventBus,
    eventIdGenerator: EventIdGenerator,
    newTaskRunner: TaskRunner.Factory,
    executionContext: ExecutionContext)
extends KeyedEventJournalingActor[AgentEvent] {

  override val supervisorStrategy = StoppingStrategies.stopping(logger)
  private val journalFile = stateDirectory / "journal"
  protected val journalActor = context.actorOf(
    Props { new JsonJournalActor(MyJournalMeta, journalFile, syncOnCommit = syncOnCommit, eventIdGenerator, keyedEventBus) },
    "Journal")
  private val jobKeeper = context.actorOf(Props { new JobKeeper(jobConfigurationDirectory) }, "JobKeeper")
  private val masterToOrderKeeper = new MasterRegister
  private var terminating = false

  def snapshots = Future.successful(masterToOrderKeeper.keys map AgentSnapshot.Master.apply)

  override def preStart() = {
    super.preStart()
    new MyJournalRecoverer().recoverAllAndSendTo(journalActor = journalActor)
  }

  override def postStop() = {
    logger.info("Agent is terminating now")
    super.postStop()
    stoppedPromise.success(Completed)
  }

  private class MyJournalRecoverer extends JsonJournalRecoverer[AgentEvent] {
    val jsonJournalMeta = MyJournalMeta
    val journalFile = AgentActor.this.journalFile

    def recoverSnapshot = {
      case AgentSnapshot.Master(userId) ⇒
        addOrderKeeper(userId)
    }

    def recoverNewKey = {
      case Stamped(_, e @ KeyedEvent(_: UserId, AgentEvent.MasterAdded)) ⇒
        val keyedEvent = e.asInstanceOf[KeyedEvent[AgentEvent.MasterAdded.type]]
        update(keyedEvent)
    }
  }

  def receive = journaling orElse {
    case JsonJournalRecoverer.Output.JournalIsReady ⇒
      if (masterToOrderKeeper.nonEmpty) {
        logger.info(s"${masterToOrderKeeper.size} recovered master registrations: ${masterToOrderKeeper.keys.mkString(", ")}")
      }
      context.become(startable)
      unstashAll()

    case _ ⇒
      stash()
  }

  private def startable: Receive = journaling orElse {
    case Input.Start ⇒
      context.become(startingJobKeeper(sender()))
      jobKeeper ! JobKeeper.Input.Start
  }

  private def startingJobKeeper(commander: ActorRef): Receive = journaling orElse {
    case JobKeeper.Output.Ready(jobs) ⇒
      for (a ← masterToOrderKeeper.values) a ! AgentOrderKeeper.Input.Start(jobs)  // Start recovered actors
      context.become(ready(jobs))
      commander ! Output.Started
  }

  private def ready(jobs: Seq[(JobPath, ActorRef)]): Receive = journaling orElse {
    case cmd: Input.ExternalCommand ⇒
      executeCommand(cmd, jobs)

    case Input.RequestEvents(userId, input) ⇒
      masterToOrderKeeper.get(userId) match {
        case Some(actor) ⇒ actor ! (input: AgentOrderKeeper.Input)
        case None ⇒ sender() ! Status.Failure(new NoSuchElementException(s"No Master registered for User '$userId'"))
      }

    case msg: JobRunner.Output.ReadyForOrder.type ⇒
      for (actor ← masterToOrderKeeper.values) {
        actor.forward(msg)
      }

    case Terminated(a) if masterToOrderKeeper.contains(a) && terminating ⇒
      masterToOrderKeeper -= a
      handleActorTermination()
  }

  private def executeCommand(externalCommand: Input.ExternalCommand, jobs: Seq[(JobPath, ActorRef)]): Unit = {
    import externalCommand.{command, response, userId}
    command match {
      case command: AgentCommand.Terminate ⇒
        terminating = true
        terminateOrderKeepers() onComplete { orderTried ⇒
          (jobKeeper ? command).mapTo[EmptyResponse.type] onComplete { j ⇒
            response.complete(orderTried flatMap { _ ⇒ j })
          }
        }
        handleActorTermination()

      case AgentCommand.RegisterAsMaster if !terminating ⇒
        //??? require(sessionToken.isDefined)
        if (masterToOrderKeeper contains userId) {
          response.success(EmptyResponse)
        } else {
          persist(KeyedEvent(AgentEvent.MasterAdded)(userId)) { case Stamped(_, keyedEvent) ⇒
            update(keyedEvent)
            masterToOrderKeeper(userId) ! AgentOrderKeeper.Input.Start(jobs)
            response.success(EmptyResponse)
          }
        }

      case command: AgentCommand.OrderCommand ⇒
        masterToOrderKeeper.get(userId) match {
          case Some(actor) ⇒
            actor.forward(AgentOrderKeeper.Input.ExternalCommand(command, response))
          case None ⇒
            response.failure(new NoSuchElementException(s"Unknown Master for User '$userId'"))
        }

      case _ if terminating ⇒
        response.failure(new IllegalStateException(s"Agent is terminating"))
    }
  }

  private def terminateOrderKeepers(): Future[EmptyResponse.type] =
    Future.sequence(
      (for (a ← masterToOrderKeeper.values) yield
        (a ? AgentOrderKeeper.Input.Terminate).mapTo[Done]))
    .map { _ ⇒ EmptyResponse }

  private def update(keyedEvent: KeyedEvent[AgentEvent]): Unit = {
    keyedEvent match {
      case KeyedEvent(userId: UserId, AgentEvent.MasterAdded) ⇒
        addOrderKeeper(userId)
    }
  }

  private def addOrderKeeper(userId: UserId): ActorRef = {
    val actor = context.actorOf(
      Props {
        new AgentOrderKeeper(
          journalFile = stateDirectory / s"master-$userId.journal",
          askTimeout = askTimeout,
          syncOnCommit = syncOnCommit,
          keyedEventBus,
          eventIdGenerator,
          timerService)
        },
      Akkas.encodeAsActorName(s"AgentOrderKeeper-for-$userId"))
    masterToOrderKeeper.insert(userId → actor)
    context.watch(actor)
  }

  private def handleActorTermination(): Unit = {
    if (masterToOrderKeeper.isEmpty) {
      context.stop(self)
    }
  }

  override def toString = "AgentActor"
}

object AgentActor {
  private val logger = Logger(getClass)

  sealed trait Input
  object Input {
    final case object Start extends Output
    final case class ExternalCommand(userId: UserId, command: AgentCommand, response: Promise[AgentCommand.Response])
    final case class RequestEvents(usedId: UserId, input: AgentOrderKeeper.Input.RequestEvents)
  }

  sealed trait Output
  object Output {
    case object Started extends Output
  }

  val MyJournalMeta = new JsonJournalMeta(
      AgentSnapshot.jsonFormat,
      AgentEvent.KeyedEventJsonFormat,
      snapshotToKey = {
        case master: AgentSnapshot.Master ⇒ master.userId
      },
      isDeletedEvent = Set())
    with GzipCompression

  private final class MasterRegister extends ActorRegister[UserId, ActorRef](identity) {
    override def onUnknownKey(userId: UserId) =
      throw new NoSuchElementException(s"No master registered for user '$userId'")

    override def insert(kv: (UserId, ActorRef)) = super.insert(kv)

    override def -=(a: ActorRef) = super.-=(a)
  }
}
