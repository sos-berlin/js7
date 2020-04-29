package com.sos.jobscheduler.agent.scheduler

import akka.actor.{ActorRef, PoisonPill, Props, Terminated}
import akka.pattern.{ask, pipe}
import cats.data.EitherT
import com.sos.jobscheduler.agent.AgentState
import com.sos.jobscheduler.agent.configuration.{AgentConfiguration, AgentStartInformation}
import com.sos.jobscheduler.agent.data.AgentTermination
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.CoupleMaster
import com.sos.jobscheduler.agent.data.event.KeyedEventJsonFormats.AgentKeyedEventJsonCodec
import com.sos.jobscheduler.agent.data.problems.{MasterAgentMismatchProblem, UnknownMasterProblem}
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
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.Assertions.assertThat
import com.sos.jobscheduler.base.utils.Closer.syntax._
import com.sos.jobscheduler.base.utils.{Closer, SetOnce}
import com.sos.jobscheduler.common.akkautils.{Akkas, SupervisorStrategies}
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.JavaInformations.javaInformation
import com.sos.jobscheduler.common.system.SystemInformations.systemInformation
import com.sos.jobscheduler.core.common.ActorRegister
import com.sos.jobscheduler.core.crypt.generic.GenericSignatureVerifier
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.core.event.journal.data.JournalMeta
import com.sos.jobscheduler.core.event.journal.recover.JournalRecoverer
import com.sos.jobscheduler.core.event.journal.watch.JournalEventWatch
import com.sos.jobscheduler.core.event.journal.{JournalActor, MainJournalingActor}
import com.sos.jobscheduler.core.event.state.JournaledStatePersistence
import com.sos.jobscheduler.data.agent.AgentRunId
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{EventId, JournalEvent, JournalId, JournalState, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.workflow.Workflow
import javax.inject.{Inject, Singleton}
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.{Future, Promise}
import shapeless.tag

/**
  * @author Joacim Zschimmer
  */
private[agent] final class AgentActor @Inject private(
  terminatePromise: Promise[AgentTermination.Terminate],
  agentConfiguration: AgentConfiguration,
  newTaskRunner: TaskRunner.Factory,
  eventIdGenerator: EventIdGenerator,
  keyedEventBus: StampedKeyedEventBus)
  (implicit closer: Closer, scheduler: Scheduler)
extends MainJournalingActor[AgentServerState, AgentEvent] {

  import agentConfiguration.{akkaAskTimeout, stateDirectory}
  import context.{actorOf, watch}

  override val supervisorStrategy = SupervisorStrategies.escalate

  private val journalMeta = JournalMeta(AgentServerJsonCodecs.jsonCodec, AgentEvent.KeyedEventJsonCodec, stateDirectory / "agent")
  protected val journalActor = tag[JournalActor.type](watch(actorOf(
    JournalActor.props[AgentServerState](journalMeta, agentConfiguration.journalConf, keyedEventBus, scheduler, eventIdGenerator),
    "Journal")))
  private var state = AgentServerState.empty
  private val masterToOrderKeeper = new MasterRegister
  private val signatureVerifier = GenericSignatureVerifier(agentConfiguration.config).orThrow
  private val shutDownCommand = SetOnce[AgentCommand.ShutDown]
  private def terminating = shutDownCommand.isDefined
  private val terminateCompleted = Promise[Completed]()

  def snapshots = state.toSnapshotObservable.toListL.runToFuture

  override def preStart() = {
    super.preStart()
    val recoverer = new MyJournalRecoverer()
    recoverer.recoverAll()
    recoverer.startJournalAndFinishRecovery(journalActor, AgentServerState.empty)
  }

  override def postStop() = {
    for (m <- masterToOrderKeeper.values) m.eventWatch.close()
    super.postStop()
    terminatePromise.trySuccess(AgentTermination.Terminate(restart = shutDownCommand.fold(false)(_.restart)))
    logger.debug("Stopped")
  }

  private class MyJournalRecoverer extends JournalRecoverer[AgentServerState]
  {
    protected val journalMeta = AgentActor.this.journalMeta
    protected val expectedJournalId = None

    def recoverSnapshot = {
      case snapshot @ RegisteredMaster(masterId, agentRunId) =>
        state = state.applySnapshot(snapshot).orThrow
        addOrderKeeper(masterId, agentRunId)
    }

    def recoverEvent = {
      case Stamped(_, _, KeyedEvent(_: NoKey, event: AgentEvent)) =>
        update(event)

      case Stamped(_, _, KeyedEvent(_, _: JournalEvent)) =>
    }
  }

  def receive = {
    case JournalRecoverer.Output.JournalIsReady(_) =>
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
        case Right(entry) => entry.eventWatch.whenStarted.map(Right.apply) pipeTo sender()
        case o @ Left(_) => sender() ! o
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
      case command: AgentCommand.ShutDown =>
        if (!terminating) {
          shutDownCommand := command
          terminateOrderKeepers(command) onComplete { ordersTerminated =>
            response.complete(ordersTerminated map Right.apply)
            terminateCompleted.success(Completed)  // Wait for child Actor termination
            continueTermination()
          }
        }

      case AgentCommand.RegisterAsMaster if !terminating =>
        state.idToMaster.get(masterId) match {
          case None =>
            val agentRunId = AgentRunId(JournalId.random())
            response completeWith
              persist(AgentEvent.MasterRegistered(masterId, agentRunId)) {
                case (Stamped(_, _, KeyedEvent(NoKey, event)), journaledState) =>
                  update(event)
                  Right(AgentCommand.RegisterAsMaster.Response(agentRunId))
                }

          case Some(registeredMaster) =>
            response.completeWith(
              checkMaster(masterId, None, EventId.BeforeFirst)
                .map(_.map((_: MasterRegister.Entry) => AgentCommand.RegisterAsMaster.Response(registeredMaster.agentRunId)))
                .runToFuture)
        }

      case AgentCommand.CoupleMaster(agentRunId, eventId) if !terminating =>
        // Command does not change state. It only checks the coupling (for now)
        response.completeWith(
          ( for {
              entry <- EitherT(checkMaster(masterId, Some(agentRunId), eventId))
              response <- EitherT(entry.persistence.currentState
                .map(state => Checked(CoupleMaster.Response(state.idToOrder.keySet))))
            } yield response
          ).value
            .runToFuture)

      case command @ (_: AgentCommand.OrderCommand | _: AgentCommand.TakeSnapshot.type) =>
        masterToOrderKeeper.checked(masterId) match {
          case Right(entry) =>
            entry.actor.forward(AgentOrderKeeper.Input.ExternalCommand(command, response))
          case Left(problem) =>
            response.failure(problem.throwable)
        }

      case _ if terminating =>
        response.failure(AgentIsShuttingDownProblem.throwable)
    }
  }

  private def checkMaster(masterId: MasterId, requiredAgentRunId: Option[AgentRunId], eventId: EventId): Task[Checked[MasterRegister.Entry]] = {
    def pure[A](a: Checked[A]) = EitherT(Task.pure(a))
    ( for {
        registeredMaster <- pure(state.idToMaster.get(masterId).toChecked(UnknownMasterProblem(masterId)))
        entry <- pure(masterToOrderKeeper.checked(masterId))
        eventWatch <- EitherT(Task.fromFuture(entry.eventWatch.whenStarted) map Checked.apply)
        _ <- pure {
            assertThat(entry.agentRunId == registeredMaster.agentRunId)
            Checked.cond(requiredAgentRunId.forall(_ == registeredMaster.agentRunId), (), MasterAgentMismatchProblem)
          }
        _ <- pure(eventWatch.checkEventId(eventId))
      } yield entry
    ).value
  }

  private def continueTermination(): Unit =
    if (terminating && masterToOrderKeeper.isEmpty) {
      journalActor ! JournalActor.Input.Terminate
    }

  private def terminateOrderKeepers(terminate: AgentCommand.ShutDown): Future[AgentCommand.Response.Accepted] =
    Future.sequence(
      for (a <- masterToOrderKeeper.values.map(_.actor)) yield
        (a ? terminate).mapTo[AgentCommand.Response.Accepted])
    .map { _ => AgentCommand.Response.Accepted }

  private def update(event: AgentEvent): Unit = {
    state = state.applyEvent(event).orThrow
    event match {
      case AgentEvent.MasterRegistered(masterId, agentRunId) =>
        addOrderKeeper(masterId, agentRunId)
    }
  }

  private def addOrderKeeper(masterId: MasterId, agentRunId: AgentRunId): ActorRef = {
    val journalMeta = JournalMeta(SnapshotJsonFormat, AgentKeyedEventJsonCodec, stateDirectory / s"master-$masterId")
    val recovered = OrderJournalRecoverer.recover(journalMeta, agentRunId.journalId, agentConfiguration)  // May take minutes !!!
    recovered.closeWithCloser
    val journalActor = tag[JournalActor.type](actorOf(
      JournalActor.props[AgentState](journalMeta, agentConfiguration.journalConf, keyedEventBus, scheduler, eventIdGenerator),
      Akkas.encodeAsActorName(s"JournalActor-for-$masterId")))
    val persistence = new JournaledStatePersistence[AgentState](journalActor)
    val actor = actorOf(
      Props {
        new AgentOrderKeeper(
          masterId,
          recovered,
          signatureVerifier,
          newTaskRunner,
          persistence,
          askTimeout = akkaAskTimeout,
          agentConfiguration)
        },
      Akkas.encodeAsActorName(s"AgentOrderKeeper-for-$masterId"))
    masterToOrderKeeper.insert(masterId -> MasterRegister.Entry(masterId, agentRunId, persistence, actor, recovered.eventWatch))
    watch(actor)
  }

  override def toString = "AgentActor"
}

object AgentActor
{
  private val logger = Logger(getClass)
  private val SnapshotJsonFormat = TypedJsonCodec[Any](
    Subtype[JournalState],
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
    override protected def noSuchKeyProblem(masterId: MasterId) = UnknownMasterProblem(masterId)

    override def insert(kv: (MasterId, MasterRegister.Entry)) = super.insert(kv)

    override def -=(a: ActorRef) = super.-=(a)
  }
  object MasterRegister {
    final case class Entry(
      masterId: MasterId,
      agentRunId: AgentRunId,
      persistence: JournaledStatePersistence[AgentState],
      actor: ActorRef,
      eventWatch: JournalEventWatch)
  }

  @Singleton
  final class Factory @Inject private(
    agentConfiguration: AgentConfiguration,
    newTaskRunner: TaskRunner.Factory,
    eventIdGenerator: EventIdGenerator,
    keyedEventBus: StampedKeyedEventBus)
    (implicit closer: Closer, scheduler: Scheduler)
  {
    def apply(terminatePromise: Promise[AgentTermination.Terminate]) =
      new AgentActor(terminatePromise, agentConfiguration, newTaskRunner, eventIdGenerator, keyedEventBus)
  }
}
