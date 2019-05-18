package com.sos.jobscheduler.agent.scheduler

import akka.actor.{ActorRef, PoisonPill, Props, Status, Terminated}
import akka.pattern.ask
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.agent.configuration.{AgentConfiguration, AgentStartInformation}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.views.AgentOverview
import com.sos.jobscheduler.agent.scheduler.AgentActor._
import com.sos.jobscheduler.agent.scheduler.job.JobActor
import com.sos.jobscheduler.agent.scheduler.job.task.TaskRunner
import com.sos.jobscheduler.agent.scheduler.order.AgentOrderKeeper
import com.sos.jobscheduler.agent.scheduler.problems.AgentIsShuttingDownProblem
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.akkautils.{Akkas, SupervisorStrategies}
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.{IOExecutor, Logger}
import com.sos.jobscheduler.common.system.JavaInformations.javaInformation
import com.sos.jobscheduler.common.system.SystemInformations.systemInformation
import com.sos.jobscheduler.core.common.ActorRegister
import com.sos.jobscheduler.core.crypt.generic.GenericSignatureVerifier
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer
import com.sos.jobscheduler.core.event.journal.{JournalActor, MainJournalingActor}
import com.sos.jobscheduler.core.problems.NoSuchMasterProblem
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{KeyedEvent, Stamped}
import com.sos.jobscheduler.data.master.MasterId
import javax.inject.Inject
import monix.execution.Scheduler
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
private[agent] final class AgentActor @Inject private(
  agentConfiguration: AgentConfiguration,
  newTaskRunner: TaskRunner.Factory,
  keyedEventBus: StampedKeyedEventBus)
  (implicit scheduler: Scheduler, iox: IOExecutor)
extends MainJournalingActor[AgentEvent] {

  import agentConfiguration.{akkaAskTimeout, stateDirectory}
  import context.{actorOf, watch}

  override val supervisorStrategy = SupervisorStrategies.escalate

  private val journalMeta = JournalMeta(AgentSnapshot.jsonCodec, AgentEvent.KeyedEventJsonCodec, stateDirectory / "agent")
  protected val journalActor = watch(actorOf(
    JournalActor.props(journalMeta, agentConfiguration.config, keyedEventBus, scheduler),
    "Journal"))
  private val masterToOrderKeeper = new MasterRegister
  private val signatureVerifier = GenericSignatureVerifier(agentConfiguration.config).orThrow
  private var terminating = false
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
    logger.debug("Stopped")
  }

  private class MyJournalRecoverer extends JournalRecoverer[AgentEvent] {
    protected val sender = AgentActor.this.sender()
    protected val journalMeta = AgentActor.this.journalMeta

    def recoverSnapshot = {
      case AgentSnapshot.Master(masterId) =>
        addOrderKeeper(masterId)
    }

    def recoverEvent = {
      case Stamped(_, _, KeyedEvent(NoKey, event: AgentEvent)) =>
        update(event)
    }
  }

  def receive = {
    case JournalRecoverer.Output.JournalIsReady =>
      if (masterToOrderKeeper.nonEmpty) {
        logger.info(s"${masterToOrderKeeper.size} recovered master registrations: ${masterToOrderKeeper.keys.mkString(", ")}")
      }
      become("startable")(startable)
      unstashAll()

    case _ =>
      stash()
  }

  private def startable: Receive = {
    case Input.Start =>
      become("ready")(ready)
      sender() ! Output.Ready
  }

  private def ready: Receive = {
    case cmd: Input.ExternalCommand =>
      executeExternalCommand(cmd)

    case Input.GetEventWatch(masterId) =>
      masterToOrderKeeper.checked(masterId) match {
        case Valid(actor) => actor.forward(AgentOrderKeeper.Input.GetEventWatch)
        case Invalid(problem) => sender() ! Status.Failure(problem.throwable)
      }

    case msg: JobActor.Output.ReadyForOrder.type =>
      for (actor <- masterToOrderKeeper.values) {
        actor.forward(msg)
      }

    case Terminated(a) if masterToOrderKeeper.contains(a) =>
      logger.debug("Actor for master " + masterToOrderKeeper.actorToKey(a) + " terminated")
      masterToOrderKeeper -= a
      continueTermination()

    case Terminated(`journalActor`) if terminating =>
      for (_ <- terminateCompleted.future) context.self ! PoisonPill

    case Command.GetOverview =>
      sender() ! AgentOverview(
        version = AgentStartInformation.PrettyVersion,
        buildId = AgentStartInformation.BuildId,
        startedAt = AgentStartInformation.StartedAt,
        isTerminating = terminating,
        system = systemInformation(),
        java = javaInformation)
  }

  private def executeExternalCommand(externalCommand: Input.ExternalCommand): Unit = {
    import externalCommand.{command, response, userId}
    val masterId = MasterId.fromUserId(userId)
    command match {
      case command: AgentCommand.Terminate =>
        if (!terminating) {
          terminating = true
          terminateOrderKeepers(command) onComplete { ordersTerminated =>
            response.complete(ordersTerminated map Valid.apply)
            terminateCompleted.success(Completed)  // Wait for child Actor termination
            continueTermination()
          }
        }

      case AgentCommand.RegisterAsMaster if !terminating =>
        if (masterToOrderKeeper contains masterId) {
          response.success(Valid(AgentCommand.Response.Accepted))
        } else {
          response completeWith
            persist(AgentEvent.MasterAdded(masterId)) { case Stamped(_, _, KeyedEvent(NoKey, event)) =>
              update(event)
              Valid(AgentCommand.Response.Accepted)
            }
        }

      case command: AgentCommand.OrderCommand =>
        masterToOrderKeeper.checked(masterId) match {
          case Valid(actor) =>
            actor.forward(AgentOrderKeeper.Input.ExternalCommand(command, response))
          case Invalid(problem) =>
            response.failure(problem.throwable)
        }

      case _ if terminating =>
        response.failure(AgentIsShuttingDownProblem.throwable)
    }
  }

  private def continueTermination(): Unit =
    if (terminating && masterToOrderKeeper.isEmpty) {
      journalActor ! JournalActor.Input.Terminate
    }

  private def terminateOrderKeepers(terminate: AgentCommand.Terminate): Future[AgentCommand.Response.Accepted] =
    Future.sequence(
      for (a <- masterToOrderKeeper.values) yield
        (a ? terminate).mapTo[AgentCommand.Response.Accepted])
    .map { _ => AgentCommand.Response.Accepted }

  private def update(event: AgentEvent): Unit =
    event match {
      case AgentEvent.MasterAdded(masterId) =>
        addOrderKeeper(masterId)
    }

  private def addOrderKeeper(masterId: MasterId): ActorRef = {
    val actor = actorOf(
      Props {
        new AgentOrderKeeper(
          masterId,
          journalFileBase = stateDirectory / s"master-$masterId",
          signatureVerifier,
          newTaskRunner,
          askTimeout = akkaAskTimeout,
          keyedEventBus,
          agentConfiguration)
        },
      Akkas.encodeAsActorName(s"AgentOrderKeeper-for-$masterId"))
    masterToOrderKeeper.insert(masterId -> actor)
    watch(actor)
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
    final case class ExternalCommand(userId: UserId, command: AgentCommand, response: Promise[Checked[AgentCommand.Response]])
    final case class GetEventWatch(masterId: MasterId)
  }

  object Output {
    case object Ready
  }

  private final class MasterRegister extends ActorRegister[MasterId, ActorRef](identity) {
    override protected def noSuchKeyProblem(masterId: MasterId) = NoSuchMasterProblem(masterId)

    override def insert(kv: (MasterId, ActorRef)) = super.insert(kv)

    override def -=(a: ActorRef) = super.-=(a)
  }
}
