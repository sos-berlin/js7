package js7.agent.scheduler

import akka.actor.{ActorRef, PoisonPill, Props, Terminated}
import akka.pattern.{ask, pipe}
import cats.data.EitherT
import javax.inject.{Inject, Singleton}
import js7.agent.configuration.{AgentConfiguration, AgentStartInformation}
import js7.agent.data.AgentTermination
import js7.agent.data.Problems.{AgentIsShuttingDown, ControllerAgentMismatch, DuplicateAgentRef, UnknownController}
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.CoupleController
import js7.agent.data.event.KeyedEventJsonFormats.AgentKeyedEventJsonCodec
import js7.agent.data.views.AgentOverview
import js7.agent.scheduler.AgentActor._
import js7.agent.scheduler.job.JobActor
import js7.agent.scheduler.job.task.TaskRunner
import js7.agent.scheduler.order.AgentOrderKeeper
import js7.agent.{AgentState, AgentStateBuilder}
import js7.base.auth.UserId
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.generic.Completed
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked
import js7.base.problem.Checked._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.{Closer, SetOnce}
import js7.common.akkautils.{Akkas, SupervisorStrategies}
import js7.common.event.EventIdGenerator
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Logger
import js7.common.system.JavaInformations.javaInformation
import js7.common.system.SystemInformations.systemInformation
import js7.core.common.ActorRegister
import js7.core.crypt.generic.GenericSignatureVerifier
import js7.core.event.StampedKeyedEventBus
import js7.core.event.journal.data.JournalMeta
import js7.core.event.journal.recover.{JournaledStateRecoverer, Recovered}
import js7.core.event.journal.watch.JournalEventWatch
import js7.core.event.journal.{JournalActor, MainJournalingActor}
import js7.core.event.state.JournaledStatePersistence
import js7.data.agent.{AgentRefPath, AgentRunId}
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, JournalId, JournalState, KeyedEvent, Stamped}
import js7.data.order.Order
import js7.data.workflow.Workflow
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
  (implicit closer: Closer, protected val scheduler: Scheduler)
extends MainJournalingActor[AgentServerState, AgentEvent] {

  import agentConfiguration.{akkaAskTimeout, stateDirectory}
  import context.{actorOf, watch}

  override val supervisorStrategy = SupervisorStrategies.escalate
  protected def journalConf = agentConfiguration.journalConf

  private val journalMeta = JournalMeta(AgentServerJsonCodecs.jsonCodec, AgentEvent.KeyedEventJsonCodec, stateDirectory / "agent")
  protected val journalActor = tag[JournalActor.type](watch(actorOf(
    JournalActor.props[AgentServerState](journalMeta, agentConfiguration.journalConf, keyedEventBus, scheduler, eventIdGenerator),
    "Journal")))
  private var state = AgentServerState.empty
  private val controllerToOrderKeeper = new ControllerRegister
  private val signatureVerifier = GenericSignatureVerifier(agentConfiguration.config).orThrow
  private val shutDownCommand = SetOnce[AgentCommand.ShutDown]
  private def terminating = shutDownCommand.isDefined
  private val terminateCompleted = Promise[Completed]()

  def snapshots = state.toSnapshotObservable.toL(Vector).runToFuture

  override def preStart() = {
    super.preStart()
    val recovered = JournaledStateRecoverer.recover[AgentServerState](
      journalMeta,
      AgentServerState.empty,
      () => new AgentServerStateBuilder,
      agentConfiguration.config)
    state = recovered.state
    for (o <- state.idToController.values) {
      addOrderKeeper(o.controllerId, o.agentRefPath, o.agentRunId)
    }
    recovered.startJournalAndFinishRecovery(journalActor)
  }

  override def postStop() = {
    for (m <- controllerToOrderKeeper.values) m.eventWatch.close()
    super.postStop()
    terminatePromise.trySuccess(AgentTermination.Terminate(restart = shutDownCommand.fold(false)(_.restart)))
    logger.debug("Stopped")
  }

  def receive = {
    case Recovered.Output.JournalIsReady(_) =>
      if (controllerToOrderKeeper.nonEmpty) {
        logger.info(s"${controllerToOrderKeeper.size} recovered Controller registrations: ${controllerToOrderKeeper.keys.mkString(", ")}")
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

    case Input.GetEventWatch(controllerId) =>
      controllerToOrderKeeper.checked(controllerId) match {
        case Right(entry) => entry.eventWatch.whenStarted.map(Right.apply) pipeTo sender()
        case o @ Left(_) => sender() ! o
      }

    case msg: JobActor.Output.ReadyForOrder.type =>
      for (entry <- controllerToOrderKeeper.values) {
        entry.actor.forward(msg)
      }

    case Terminated(a) if controllerToOrderKeeper.contains(a) =>
      logger.debug("Actor for controller " + controllerToOrderKeeper.actorToKey(a) + " terminated")
      controllerToOrderKeeper -= a
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
    val controllerId = ControllerId.fromUserId(userId)
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

      case AgentCommand.RegisterAsController(agentRefPath) if !terminating =>
        state.idToController.get(controllerId) match {
          case None =>
            val agentRunId = AgentRunId(JournalId.random())
            response completeWith
              persist(AgentEvent.ControllerRegistered(controllerId, agentRefPath, agentRunId)) {
                case (Stamped(_, _, KeyedEvent(NoKey, event)), _) =>
                  update(event)
                  Right(AgentCommand.RegisterAsController.Response(agentRunId))
                }

          case Some(registeredController) =>
            response.completeWith(
              checkController(controllerId, agentRefPath, None, EventId.BeforeFirst)
                .map(_.map((_: ControllerRegister.Entry) => AgentCommand.RegisterAsController.Response(registeredController.agentRunId)))
                .runToFuture)
        }

      case AgentCommand.CoupleController(agentRefPath, agentRunId, eventId) if !terminating =>
        // Command does not change state. It only checks the coupling (for now)
        response.completeWith(
          ( for {
              entry <- EitherT(checkController(controllerId, agentRefPath, Some(agentRunId), eventId))
              response <- EitherT(entry.persistence.currentState
                .map(state => Checked(CoupleController.Response(state.idToOrder.keySet))))
            } yield response
          ).value
            .runToFuture)

      case command @ (_: AgentCommand.OrderCommand | _: AgentCommand.TakeSnapshot.type) =>
        controllerToOrderKeeper.checked(controllerId) match {
          case Right(entry) =>
            entry.actor.forward(AgentOrderKeeper.Input.ExternalCommand(command, response))
          case Left(problem) =>
            response.failure(problem.throwable)
        }

      case _ if terminating =>
        response.failure(AgentIsShuttingDown.throwable)
    }
  }

  private def checkController(controllerId: ControllerId, requestedAgentRefPath: AgentRefPath, requestedAgentRunId: Option[AgentRunId], eventId: EventId)
  : Task[Checked[ControllerRegister.Entry]] = {
    def pure[A](a: Checked[A]) = EitherT(Task.pure(a))
    ( for {
        registeredController <- pure(state.idToController.get(controllerId).toChecked(UnknownController(controllerId)))
        entry <- pure(controllerToOrderKeeper.checked(controllerId))
        eventWatch <- EitherT(entry.eventWatch.started map Checked.apply)
        _ <- pure {
            assertThat(entry.agentRunId == registeredController.agentRunId)
            if (requestedAgentRefPath != registeredController.agentRefPath)
              Left(DuplicateAgentRef(first = registeredController.agentRefPath, second = requestedAgentRefPath))
            else if (!requestedAgentRunId.forall(_ == registeredController.agentRunId))
              Left(ControllerAgentMismatch(registeredController.agentRefPath))
            else
              Right(())
          }
        _ <- pure(eventWatch.checkEventId(eventId))
      } yield entry
    ).value
  }

  private def continueTermination(): Unit =
    if (terminating && controllerToOrderKeeper.isEmpty) {
      journalActor ! JournalActor.Input.Terminate
    }

  private def terminateOrderKeepers(terminate: AgentCommand.ShutDown): Future[AgentCommand.Response.Accepted] =
    Future.sequence(
      for (a <- controllerToOrderKeeper.values.map(_.actor)) yield
        (a ? terminate).mapTo[AgentCommand.Response.Accepted]
    ).map(_ => AgentCommand.Response.Accepted)

  private def update(event: AgentEvent): Unit = {
    state = state.applyEvent(event).orThrow
    event match {
      case AgentEvent.ControllerRegistered(controllerId, agentRefPath, agentRunId) =>
        addOrderKeeper(controllerId, agentRefPath, agentRunId)
    }
  }

  private def addOrderKeeper(controllerId: ControllerId, agentRefPath: AgentRefPath, agentRunId: AgentRunId): ActorRef = {
    val journalMeta = JournalMeta(SnapshotJsonFormat, AgentKeyedEventJsonCodec, stateDirectory / s"controller-$controllerId")

    // May take minutes !!!
    val recovered = JournaledStateRecoverer.recover[AgentState](
      journalMeta,
      AgentState.empty,
      () => new AgentStateBuilder,
      agentConfiguration.config)

    val journalActor = tag[JournalActor.type](actorOf(
      JournalActor.props[AgentState](journalMeta, agentConfiguration.journalConf, keyedEventBus, scheduler, eventIdGenerator),
      Akkas.encodeAsActorName(s"JournalActor-for-$controllerId")))
    val persistence = new JournaledStatePersistence[AgentState](journalActor, agentConfiguration.journalConf)
    val actor = actorOf(
      Props {
        new AgentOrderKeeper(
          controllerId,
          agentRefPath,
          recovered,
          signatureVerifier,
          newTaskRunner,
          persistence,
          askTimeout = akkaAskTimeout,
          agentConfiguration)
        },
      Akkas.encodeAsActorName(s"AgentOrderKeeper-for-$controllerId"))
    controllerToOrderKeeper.insert(controllerId -> ControllerRegister.Entry(controllerId, agentRunId, persistence, actor, recovered.eventWatch))
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
    final case class GetEventWatch(controllerId: ControllerId)
  }

  object Output {
    case object Ready
  }

  private final class ControllerRegister extends ActorRegister[ControllerId, ControllerRegister.Entry](_.actor) {
    override protected def noSuchKeyProblem(controllerId: ControllerId) = UnknownController(controllerId)

    override def insert(kv: (ControllerId, ControllerRegister.Entry)) = super.insert(kv)

    override def -=(a: ActorRef) = super.-=(a)
  }
  object ControllerRegister {
    final case class Entry(
      controllerId: ControllerId,
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
