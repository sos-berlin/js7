package com.sos.jobscheduler.agent.scheduler

import akka.actor.{ActorRef, PoisonPill, Props, Terminated}
import akka.pattern.{ask, pipe}
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.agent.configuration.{AgentConfiguration, AgentStartInformation}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.Response
import com.sos.jobscheduler.agent.data.event.KeyedEventJsonFormats.AgentKeyedEventJsonCodec
import com.sos.jobscheduler.agent.data.problems.MasterAgentMismatchProblem
import com.sos.jobscheduler.agent.data.views.AgentOverview
import com.sos.jobscheduler.agent.scheduler.AgentActor._
import com.sos.jobscheduler.agent.scheduler.job.JobActor
import com.sos.jobscheduler.agent.scheduler.job.task.TaskRunner
import com.sos.jobscheduler.agent.scheduler.order.{AgentOrderKeeper, OrderJournalRecoverer}
import com.sos.jobscheduler.agent.scheduler.problems.AgentIsShuttingDownProblem
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
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
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.core.event.journal.{JournalActor, MainJournalingActor}
import com.sos.jobscheduler.core.problems.NoSuchMasterProblem
import com.sos.jobscheduler.data.agent.AgentRunId
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{Event, EventId, JournalId, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.Workflow
import java.util.UUID.randomUUID
import javax.inject.Inject
import monix.execution.Scheduler
import scala.concurrent.{Future, Promise}
import shapeless.tag

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
  protected val journalActor = tag[JournalActor.type](watch(actorOf(
    JournalActor.props(journalMeta, agentConfiguration.journalConf, keyedEventBus, scheduler),
    "Journal")))
  private val masterToOrderKeeper = new MasterRegister
  private val signatureVerifier = GenericSignatureVerifier(agentConfiguration.config).orThrow
  private var terminating = false
  private val terminateCompleted = Promise[Completed]()

  def snapshots = Future.successful(
    masterToOrderKeeper.values.map(o => AgentSnapshot.Master(o.masterId, o.agentRunId)))

  override def preStart() = {
    super.preStart()
    val recoverer = new MyJournalRecoverer()
    recoverer.recoverAll()
    recoverer.startJournalAndFinishRecovery(journalActor)
  }

  override def postStop() = {
    for (m <- masterToOrderKeeper.values) m.eventWatch.close()
    super.postStop()
    logger.debug("Stopped")
  }

  private class MyJournalRecoverer extends JournalRecoverer[AgentEvent] {
    protected val sender = AgentActor.this.sender()
    protected val journalMeta = AgentActor.this.journalMeta
    protected val expectedJournalId = None

    def recoverSnapshot = {
      case AgentSnapshot.Master(masterId, agentRunId) =>
        addOrderKeeper(masterId, agentRunId)
    }

    def recoverEvent = {
      case Stamped(_, _, KeyedEvent(NoKey, event: AgentEvent)) =>
        update(event)
    }
  }

  def receive = {
    case JournalRecoverer.Output.JournalIsReady(_, _) =>
      if (masterToOrderKeeper.nonEmpty) {
        logger.info(s"${masterToOrderKeeper.size} recovered Master registrations: ${masterToOrderKeeper.keys.mkString(", ")}")
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
        case Valid(entry) => entry.eventWatch.whenStarted.map(Valid.apply).runToFuture pipeTo sender()
        case o @ Invalid(_) => sender() ! o
      }

    case msg: JobActor.Output.ReadyForOrder.type =>
      for (entry <- masterToOrderKeeper.values) {
        entry.actor.forward(msg)
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
        masterToOrderKeeper.get(masterId) match {
          case None =>
            val agentRunId = AgentRunId(JournalId(randomUUID))
            response completeWith
              persist(AgentEvent.MasterRegistered(masterId, agentRunId)) { case Stamped(_, _, KeyedEvent(NoKey, event)) =>
                update(event)
                Valid(AgentCommand.RegisterAsMaster.Response(agentRunId))
              }
          case Some(entry) =>
            response.completeWith(
              checkMaster(masterId, None, EventId.BeforeFirst)
                .map(_.map(_ => AgentCommand.RegisterAsMaster.Response(entry.agentRunId))))
        }

      case AgentCommand.CoupleMaster(agentRunId, eventId) if !terminating =>
        // Command does not change state. It only checks the coupling (for now)
        response.completeWith(
          checkMaster(masterId, Some(agentRunId), eventId))

      case command @ (_: AgentCommand.OrderCommand | _: AgentCommand.TakeSnapshot.type) =>
        masterToOrderKeeper.checked(masterId) match {
          case Valid(entry) =>
            entry.actor.forward(AgentOrderKeeper.Input.ExternalCommand(command, response))
          case Invalid(problem) =>
            response.failure(problem.throwable)
        }

      case _ if terminating =>
        response.failure(AgentIsShuttingDownProblem.throwable)
    }
  }

  private def checkMaster(masterId: MasterId, requiredAgentRunId: Option[AgentRunId], eventId: EventId): Future[Checked[Response.Accepted]] =
    for {
      checkedEntry <- Future.successful(masterToOrderKeeper.checked(masterId))
      checkedEventWatch <- checkedEntry.traverse(_.eventWatch.whenStarted).runToFuture
    } yield
      for {
        _ <- checkedEntry.flatMap(entry =>  Checked.cond(requiredAgentRunId.forall(_ == entry.agentRunId), (), MasterAgentMismatchProblem))
        _ <- checkedEventWatch.flatMap(_.checkEventId(eventId))
      } yield AgentCommand.Response.Accepted

  private def continueTermination(): Unit =
    if (terminating && masterToOrderKeeper.isEmpty) {
      journalActor ! JournalActor.Input.Terminate
    }

  private def terminateOrderKeepers(terminate: AgentCommand.Terminate): Future[AgentCommand.Response.Accepted] =
    Future.sequence(
      for (a <- masterToOrderKeeper.values.map(_.actor)) yield
        (a ? terminate).mapTo[AgentCommand.Response.Accepted])
    .map { _ => AgentCommand.Response.Accepted }

  private def update(event: AgentEvent): Unit =
    event match {
      case AgentEvent.MasterRegistered(masterId, agentRunId) =>
        addOrderKeeper(masterId, agentRunId)
    }

  private def addOrderKeeper(masterId: MasterId, agentRunId: AgentRunId): ActorRef = {
    val journalMeta = JournalMeta(SnapshotJsonFormat, AgentKeyedEventJsonCodec, stateDirectory / s"master-$masterId")
    val recovered = OrderJournalRecoverer.recover(journalMeta, agentRunId.journalId, agentConfiguration)  // May take minutes !!!
    val actor = actorOf(
      Props {
        new AgentOrderKeeper(
          masterId,
          recovered,
          signatureVerifier,
          newTaskRunner,
          askTimeout = akkaAskTimeout,
          keyedEventBus,
          agentConfiguration)
        },
      Akkas.encodeAsActorName(s"AgentOrderKeeper-for-$masterId"))
    masterToOrderKeeper.insert(masterId -> MasterRegister.Entry(masterId, agentRunId, recovered.eventWatch, actor))
    watch(actor)
  }

  override def toString = "AgentActor"
}

object AgentActor {
  private val logger = Logger(getClass)
  private val SnapshotJsonFormat = TypedJsonCodec[Any](
    Subtype[Workflow],
    Subtype[Order[Order.State]])

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

  private final class MasterRegister extends ActorRegister[MasterId, MasterRegister.Entry](_.actor) {
    override protected def noSuchKeyProblem(masterId: MasterId) = NoSuchMasterProblem(masterId)

    override def insert(kv: (MasterId, MasterRegister.Entry)) = super.insert(kv)

    override def -=(a: ActorRef) = super.-=(a)
  }
  object MasterRegister {
    final case class Entry(masterId: MasterId, agentRunId: AgentRunId, eventWatch: JournalEventWatch[Event], actor: ActorRef)
  }
}
