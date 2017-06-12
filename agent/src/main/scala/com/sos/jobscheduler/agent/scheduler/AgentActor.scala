package com.sos.jobscheduler.agent.scheduler

import akka.actor.{ActorRef, OneForOneStrategy, Props, Status, SupervisorStrategy}
import akka.util.Timeout
import com.sos.jobscheduler.agent.data.commandresponses.EmptyResponse
import com.sos.jobscheduler.agent.data.commands.{Command, OrderCommand, RegisterAsMaster}
import com.sos.jobscheduler.agent.scheduler.AgentActor._
import com.sos.jobscheduler.agent.scheduler.job.{JobKeeper, JobRunner}
import com.sos.jobscheduler.agent.scheduler.order.AgentOrderKeeper
import com.sos.jobscheduler.agent.task.AgentTaskFactory
import com.sos.jobscheduler.common.akkautils.Akkas
import com.sos.jobscheduler.common.auth.UserId
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.data.jobnet.JobPath
import com.sos.jobscheduler.shared.event.StampedKeyedEventBus
import com.sos.jobscheduler.shared.event.journal.JsonJournalRecoverer.{RecoveringForUnknownKey, RecoveringSnapshot}
import com.sos.jobscheduler.shared.event.journal.{GzipCompression, JsonJournalActor, JsonJournalMeta, JsonJournalRecoverer, KeyedEventJournalingActor}
import java.nio.file.Path
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
final class AgentActor(
  jobConfigurationDirectory: Path,
  stateDirectory: Path,
  implicit private val askTimeout: Timeout,
  syncOnCommit: Boolean)
  (implicit
    timerService: TimerService,
    keyedEventBus: StampedKeyedEventBus,
    eventIdGenerator: EventIdGenerator,
    newTask: AgentTaskFactory,
    executionContext: ExecutionContext)
extends KeyedEventJournalingActor[AgentEvent] {

  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 0) {
    case _ ⇒ SupervisorStrategy.Stop
  }

  private val journalFile = stateDirectory / "journal"
  protected val journalActor = context.actorOf(
    Props { new JsonJournalActor(MyJournalMeta, journalFile, syncOnCommit = syncOnCommit, eventIdGenerator, keyedEventBus) },
    "Journal")
  private val jobKeeper = context.actorOf(JobKeeper(jobConfigurationDirectory), "JobKeeper")
  private val masterToOrderKeeper = mutable.Map[UserId, ActorRef]() withDefault { userId ⇒
    throw new NoSuchElementException(s"No master registered for user '$userId'")
  }

  def snapshots = Future.successful(masterToOrderKeeper.keys.toVector map AgentSnapshot.Master.apply)

  override def preStart(): Unit = {
    super.preStart()
    recover()
  }

  private def recover(): Unit = {
    val recovered =
      autoClosing(new JsonJournalRecoverer(MyJournalMeta, journalFile)) { journal ⇒
        for (recovered ← journal) {
          (recovered: @unchecked) match {
            case RecoveringSnapshot(AgentSnapshot.Master(userId)) ⇒
              addOrderKeeper(userId)

            case RecoveringForUnknownKey(Stamped(_, KeyedEvent(userId: UserId, AgentEvent.MasterAdded))) ⇒
              addOrderKeeper(userId)
          }
        }
        journal.recoveredJournalingActors
      }
    journalActor ! JsonJournalActor.Input.Start(recovered)
  }

  def receive = journaling orElse {
    case JsonJournalActor.Output.Ready ⇒
      context.become(startable)
      logger.info(s"${masterToOrderKeeper.size} recovered master registrations: ${masterToOrderKeeper.keys.mkString(", ")}")
      unstashAll()

    case _ ⇒
      stash()
  }

  private def startable: Receive = journaling orElse {
    case Input.Start ⇒
      context.become(startingJobKeeper(sender()))
      jobKeeper ! JobKeeper.Start
  }

  private def startingJobKeeper(commander: ActorRef): Receive = journaling orElse {
    case JobKeeper.Started(jobs) ⇒
      for (a ← masterToOrderKeeper.values) a ! AgentOrderKeeper.Input.Start(jobs)  // Start recovered actors
      context.become(started(jobs))
      commander ! Output.Started
  }

  private def started(jobs: Seq[(JobPath, ActorRef)]): Receive = journaling orElse {
    case Input.CommandFromMaster(userId: UserId, command: Command) ⇒
      executeCommand(command, userId, jobs)

    case Input.RequestEvents(userId, input) ⇒
      masterToOrderKeeper.get(userId) match {
        case Some(actor) ⇒ actor ! (input: AgentOrderKeeper.Input)
        case None ⇒ sender() ! Status.Failure(new NoSuchElementException(s"No Master registered for User '$userId'"))
      }

    case msg: JobRunner.Output.ReadyForOrder.type ⇒
      for (actor ← masterToOrderKeeper.values) actor.forward(msg)   // Every MasterJunction ???
  }

  private def executeCommand(command: Command, userId: UserId, jobs: Seq[(JobPath, ActorRef)]): Unit =
    command match {
      case RegisterAsMaster ⇒
        //??? require(sessionToken.isDefined)
        if (masterToOrderKeeper contains userId) {
          sender() ! EmptyResponse
        } else {
          persist(KeyedEvent(AgentEvent.MasterAdded)(userId)) { case Stamped(_, keyedEvent) ⇒
            update(keyedEvent)
            masterToOrderKeeper(userId) ! AgentOrderKeeper.Input.Start(jobs)
            sender() ! EmptyResponse
          }
        }

      case cmd: OrderCommand ⇒
        masterToOrderKeeper.get(userId) match {
          case Some(actor) ⇒
            actor.forward(cmd)
          case None ⇒
            sender() ! Status.Failure(new NoSuchElementException(s"Unknown Master for User '$userId'"))
        }
  }

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
    masterToOrderKeeper += userId → actor
    actor
  }
}

object AgentActor {
  private val logger = Logger(getClass)

  sealed trait Input
  object Input {
    final case object Start extends Output
    final case class CommandFromMaster(usedId: UserId, command: Command)
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
}
