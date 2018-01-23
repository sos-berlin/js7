package com.sos.jobscheduler.agent.scheduler

import akka.Done
import akka.actor.{ActorRef, PoisonPill, Props, Status, Terminated}
import akka.pattern.ask
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.scheduler.AgentActor._
import com.sos.jobscheduler.agent.scheduler.job.task.TaskRunner
import com.sos.jobscheduler.agent.scheduler.job.{JobActor, JobKeeper}
import com.sos.jobscheduler.agent.scheduler.order.AgentOrderKeeper
import com.sos.jobscheduler.agent.task.{TaskRegister, TaskRegisterActor}
import com.sos.jobscheduler.agent.views.{AgentOverview, AgentStartInformation}
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.akkautils.{Akkas, SupervisorStrategies}
import com.sos.jobscheduler.common.auth.UserId
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.JavaInformations.javaInformation
import com.sos.jobscheduler.common.system.SystemInformations.systemInformation
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.data.workflow.JobPath
import com.sos.jobscheduler.shared.common.ActorRegister
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.JournalRecoverer.startJournalAndFinishRecovery
import com.sos.jobscheduler.shared.event.journal.{JournalActor, JournalMeta, JournalRecoverer, KeyedEventJournalingActor}
import javax.inject.Inject
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future, Promise}

/**
  * @author Joacim Zschimmer
  */
private[agent] final class AgentActor @Inject private(
  agentConfiguration: AgentConfiguration,
  eventIdGenerator: EventIdGenerator,
  newTaskRunner: TaskRunner.Factory,
  keyedEventBus: StampedKeyedEventBus,
  timerService: TimerService)
  (implicit executionContext: ExecutionContext)
extends KeyedEventJournalingActor[AgentEvent] {

  import agentConfiguration.{akkaAskTimeout, liveDirectory, stateDirectory}
  import context.{actorOf, watch}

  override val supervisorStrategy = SupervisorStrategies.escalate

  private val journalMeta = JournalMeta.gzipped(AgentSnapshot.jsonCodec, AgentEvent.KeyedEventJsonCodec,
    compressWithGzip = agentConfiguration.config.getBoolean("jobscheduler.agent.journal.gzip"))
  private val journalFile = stateDirectory / "journal"
  protected val journalActor = actorOf(
    JournalActor.props(journalMeta, journalFile, syncOnCommit = agentConfiguration.journalSyncOnCommit, eventIdGenerator, keyedEventBus),
    "Journal")
  private val jobKeeper = {
    val taskRegister = new TaskRegister(actorOf(TaskRegisterActor.props(agentConfiguration, timerService), "TaskRegister"))
    watch(actorOf(Props { new JobKeeper(liveDirectory, newTaskRunner, taskRegister, timerService) }, "JobKeeper"))
  }
  private val masterToOrderKeeper = new MasterRegister
  private var terminating, jobKeeperStopped = false
  private val terminateCompleted = Promise[Completed]()

  def snapshots = Future.successful(masterToOrderKeeper.keys map AgentSnapshot.Master.apply)

  override def preStart() = {
    super.preStart()
    new MyJournalRecoverer().recoverAll()
    startJournalAndFinishRecovery(journalActor = journalActor)
  }

  override def postStop() = {
    super.postStop()
    logger.info("Stopped")
  }

  private class MyJournalRecoverer extends JournalRecoverer[AgentEvent] {
    protected val sender = AgentActor.this.sender()
    protected val journalMeta = AgentActor.this.journalMeta
    protected val journalFile = AgentActor.this.journalFile

    protected def isDeletedEvent = Set()

    def recoverSnapshot = {
      case AgentSnapshot.Master(userId) ⇒
        addOrderKeeper(userId)
    }

    def recoverEvent = {
      case Stamped(_, e @ KeyedEvent(_: UserId, AgentEvent.MasterAdded)) ⇒
        val keyedEvent = e.asInstanceOf[KeyedEvent[AgentEvent.MasterAdded.type]]
        update(keyedEvent)
    }
  }

  def receive = journaling orElse {
    case JournalRecoverer.Output.JournalIsReady ⇒
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
      commander ! Output.Ready
  }

  private def ready(jobs: Seq[(JobPath, ActorRef)]): Receive = journaling orElse {
    case cmd: Input.ExternalCommand ⇒
      executeExternalCommand(cmd, jobs)

    case Input.RequestEvents(userId, input) ⇒
      masterToOrderKeeper.get(userId) match {
        case Some(actor) ⇒ actor ! (input: AgentOrderKeeper.Input)
        case None ⇒ sender() ! Status.Failure(new NoSuchElementException(s"No Master registered for User '$userId'"))
      }

    case msg: JobActor.Output.ReadyForOrder.type ⇒
      for (actor ← masterToOrderKeeper.values) {
        actor.forward(msg)
      }

    case Terminated(`jobKeeper`) ⇒
      jobKeeperStopped = true
      checkActorStop()

    case Terminated(a) if masterToOrderKeeper.contains(a) ⇒
      logger.debug("Actor for master " + masterToOrderKeeper.actorToKey(a) + " terminated")
      masterToOrderKeeper -= a
      checkActorStop()

    case Command.GetOverview ⇒
      sender() ! AgentOverview(
        version = AgentStartInformation.VersionString,
        buildId = AgentStartInformation.BuildId,
        startedAt = AgentStartInformation.StartedAt,
        isTerminating = terminating,
        system = systemInformation(),
        java = javaInformation)
  }

  private def executeExternalCommand(externalCommand: Input.ExternalCommand, jobs: Seq[(JobPath, ActorRef)]): Unit = {
    import externalCommand.{command, response, userId}
    command match {
      case command: AgentCommand.Terminate ⇒
        terminating = true
        terminateOrderKeepers() onComplete { ordersTerminated ⇒
          (jobKeeper ? command).mapTo[AgentCommand.Accepted.type] onComplete { jobsTerminated ⇒
            response.complete(ordersTerminated flatMap { _ ⇒ jobsTerminated })
            terminateCompleted.success(Completed)  // Wait for child Actor termination
          }
        }

      case AgentCommand.RegisterAsMaster if !terminating ⇒
        //??? require(sessionToken.isDefined)
        if (masterToOrderKeeper contains userId) {
          response.success(AgentCommand.Accepted)
        } else {
          persist(KeyedEvent(AgentEvent.MasterAdded)(userId)) { case Stamped(_, keyedEvent) ⇒
            update(keyedEvent)
            masterToOrderKeeper(userId) ! AgentOrderKeeper.Input.Start(jobs)
            response.success(AgentCommand.Accepted)
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

  private def terminateOrderKeepers(): Future[AgentCommand.Accepted.type] =
    Future.sequence(
      for (a ← masterToOrderKeeper.values) yield
        (a ? AgentOrderKeeper.Input.Terminate).mapTo[Done])
    .map { _ ⇒ AgentCommand.Accepted }

  private def update(keyedEvent: KeyedEvent[AgentEvent]): Unit =
    keyedEvent match {
      case KeyedEvent(userId: UserId, AgentEvent.MasterAdded) ⇒
        addOrderKeeper(userId)
    }

  private def addOrderKeeper(userId: UserId): ActorRef = {
    val actor = actorOf(
      Props {
        new AgentOrderKeeper(
          journalFile = stateDirectory / s"master-$userId.journal",
          askTimeout = akkaAskTimeout,
          syncOnCommit = agentConfiguration.journalSyncOnCommit,
          keyedEventBus,
          eventIdGenerator,
          agentConfiguration.config,
          timerService)
        },
      Akkas.encodeAsActorName(s"AgentOrderKeeper-for-$userId"))
    masterToOrderKeeper.insert(userId → actor)
    watch(actor)
  }

  private def checkActorStop(): Unit = {
    if (terminating && masterToOrderKeeper.isEmpty && jobKeeperStopped) {
      for (_ ← terminateCompleted.future) context.self ! PoisonPill
    }
  }

  override def toString = "AgentActor"
}

object AgentActor {
  private val logger = Logger(getClass)

  object Command {
    case object GetOverview
  }

  object Input {
    final case object Start
    final case class ExternalCommand(userId: UserId, command: AgentCommand, response: Promise[AgentCommand.Response])
    final case class RequestEvents(usedId: UserId, input: AgentOrderKeeper.Input.RequestEvents)
  }

  object Output {
    case object Ready
  }

  private final class MasterRegister extends ActorRegister[UserId, ActorRef](identity) {
    override def onUnknownKey(userId: UserId) =
      throw new NoSuchElementException(s"No master registered for user '$userId'")

    override def insert(kv: (UserId, ActorRef)) = super.insert(kv)

    override def -=(a: ActorRef) = super.-=(a)
  }
}
