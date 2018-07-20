package com.sos.jobscheduler.agent.scheduler

import akka.Done
import akka.actor.{ActorRef, PoisonPill, Props, Status, Terminated}
import akka.pattern.ask
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.agent.configuration.{AgentConfiguration, AgentStartInformation}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.views.AgentOverview
import com.sos.jobscheduler.agent.scheduler.AgentActor._
import com.sos.jobscheduler.agent.scheduler.AgentEvent.AgentMasterEvent
import com.sos.jobscheduler.agent.scheduler.job.task.TaskRunner
import com.sos.jobscheduler.agent.scheduler.job.{JobActor, JobKeeper}
import com.sos.jobscheduler.agent.scheduler.order.AgentOrderKeeper
import com.sos.jobscheduler.agent.task.{TaskRegister, TaskRegisterActor}
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.akkautils.{Akkas, SupervisorStrategies}
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.JavaInformations.javaInformation
import com.sos.jobscheduler.common.system.SystemInformations.systemInformation
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.common.ActorRegister
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.{JournalActor, JournalMeta, JournalRecoverer, KeyedEventJournalingActor}
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.master.MasterId
import javax.inject.Inject
import monix.execution.Scheduler
import scala.collection.immutable.Seq
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
private[agent] final class AgentActor @Inject private(
  agentConfiguration: AgentConfiguration,
  newTaskRunner: TaskRunner.Factory,
  keyedEventBus: StampedKeyedEventBus,
  timerService: TimerService)
  (implicit scheduler: Scheduler)
extends KeyedEventJournalingActor[AgentEvent] {

  import agentConfiguration.{akkaAskTimeout, fileBasedDirectory, stateDirectory}
  import context.{actorOf, watch}

  override val supervisorStrategy = SupervisorStrategies.escalate

  private val journalMeta = JournalMeta(AgentSnapshot.jsonCodec, AgentEvent.KeyedEventJsonCodec, stateDirectory / "agent")
  protected val journalActor = watch(actorOf(
    JournalActor.props(journalMeta, syncOnCommit = agentConfiguration.journalSyncOnCommit, keyedEventBus, scheduler),
    "Journal"))
  private val jobKeeper = {
    val taskRegister = new TaskRegister(actorOf(
      TaskRegisterActor.props(agentConfiguration.killScriptConf, timerService),
      "TaskRegister"))
    watch(actorOf(Props { new JobKeeper(fileBasedDirectory, newTaskRunner, taskRegister, timerService) }, "JobKeeper"))
  }
  private val masterToOrderKeeper = new MasterRegister
  private var terminating, jobKeeperStopped = false
  private val terminateCompleted = Promise[Completed]()

  def snapshots = Future.successful(masterToOrderKeeper.keys map AgentSnapshot.Master.apply)

  override def preStart() = {
    super.preStart()
    val recoverer = new MyJournalRecoverer()
    recoverer.recoverAll()
    recoverer.startJournalAndFinishRecovery(journalActor = journalActor)
  }

  override def postStop() = {
    super.postStop()
    logger.info("Stopped")
  }

  private class MyJournalRecoverer extends JournalRecoverer[AgentEvent] {
    protected val sender = AgentActor.this.sender()
    protected val journalMeta = AgentActor.this.journalMeta

    def recoverSnapshot = {
      case AgentSnapshot.Master(masterId) ⇒
        addOrderKeeper(masterId)
    }

    def recoverEvent = {
      case Stamped(_, _, e @ KeyedEvent(_: MasterId, AgentEvent.MasterAdded)) ⇒
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

    case Input.GetEventReader(masterId) ⇒
      masterToOrderKeeper.checked(masterId) match {
        case Valid(actor) ⇒ actor.forward(AgentOrderKeeper.Input.GetEventReader)
        case Invalid(problem) ⇒ sender() ! Status.Failure(problem.throwable)
      }

    case msg: JobActor.Output.ReadyForOrder.type ⇒
      for (actor ← masterToOrderKeeper.values) {
        actor.forward(msg)
      }

    case Terminated(`jobKeeper`) ⇒
      jobKeeperStopped = true
      handleTermination()

    case Terminated(a) if masterToOrderKeeper.contains(a) ⇒
      logger.debug("Actor for master " + masterToOrderKeeper.actorToKey(a) + " terminated")
      masterToOrderKeeper -= a
      handleTermination()

    case Terminated(`journalActor`) if terminating ⇒
      for (_ ← terminateCompleted.future) context.self ! PoisonPill

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
    val masterId = MasterId.fromUserId(userId)
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
        if (masterToOrderKeeper contains masterId) {
          response.success(AgentCommand.Accepted)
        } else {
          response completeWith
            persist(masterId <-: AgentEvent.MasterAdded) { case Stamped(_, _, keyedEvent) ⇒
              update(keyedEvent)
              masterToOrderKeeper(masterId) ! AgentOrderKeeper.Input.Start(jobs)
              AgentCommand.Accepted
            }
        }

      case command: AgentCommand.OrderCommand ⇒
        masterToOrderKeeper.checked(masterId) match {
          case Valid(actor) ⇒
            actor.forward(AgentOrderKeeper.Input.ExternalCommand(command, response))
          case Invalid(problem) ⇒
            response.failure(problem.throwable)
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

  private def update(keyedEvent: KeyedEvent[AgentMasterEvent]): Unit =
    keyedEvent match {
      case KeyedEvent(masterId: MasterId, AgentEvent.MasterAdded) ⇒
        addOrderKeeper(masterId)
    }

  private def addOrderKeeper(masterId: MasterId): ActorRef = {
    val actor = actorOf(
      Props {
        new AgentOrderKeeper(
          journalFileBase = stateDirectory / s"master-$masterId",
          askTimeout = akkaAskTimeout,
          syncOnCommit = agentConfiguration.journalSyncOnCommit,
          keyedEventBus,
          agentConfiguration.config,
          scheduler,
          timerService)
        },
      Akkas.encodeAsActorName(s"AgentOrderKeeper-for-$masterId"))
    masterToOrderKeeper.insert(masterId → actor)
    watch(actor)
  }

  private def handleTermination(): Unit = {
    if (terminating && masterToOrderKeeper.isEmpty && jobKeeperStopped) {
      journalActor ! JournalActor.Input.Terminate
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
    final case class GetEventReader(masterId: MasterId)
  }

  object Output {
    case object Ready
  }

  private final class MasterRegister extends ActorRegister[MasterId, ActorRef](identity) {
    override def noSuchKeyMessage(masterId: MasterId) = s"No master registered for master '$masterId'"

    override def insert(kv: (MasterId, ActorRef)) = super.insert(kv)

    override def -=(a: ActorRef) = super.-=(a)
  }
}
