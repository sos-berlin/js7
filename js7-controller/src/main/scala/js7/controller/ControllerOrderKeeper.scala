package js7.controller

import cats.effect.IO
import cats.effect.unsafe.{IORuntime, Scheduler}
import cats.instances.either.*
import cats.instances.future.*
import cats.instances.vector.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import java.time.ZoneId
import js7.agent.data.commands.AgentCommand
import js7.agent.data.event.AgentEvent
import js7.base.catsutils.CatsEffectExtensions.{materializeIntoChecked, now}
import js7.base.catsutils.SyncDeadline
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.crypt.Signed
import js7.base.eventbus.EventPublisher
import js7.base.generic.Completed
import js7.base.log.Logger.ops.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixlike.MonixLikeExtensions.{dematerialize, materialize, scheduleAtFixedRates, scheduleOnce}
import js7.base.monixlike.{SerialSyncCancelable, SyncCancelable}
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.time.{AlarmClock, Timestamp, Timezone}
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.Collections.implicits.{InsertableMutableMap, RichIterable}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.base.utils.{Allocated, ProgramTermination, SetOnce}
import js7.cluster.WorkingClusterNode
import js7.common.pekkoutils.SupervisorStrategies
import js7.common.system.startup.ServiceMain
import js7.controller.ControllerOrderKeeper.*
import js7.controller.agent.{AgentDriver, AgentDriverConfiguration}
import js7.controller.configuration.ControllerConfiguration
import js7.controller.problems.{ControllerIsNotReadyProblem, ControllerIsShuttingDownProblem, ControllerIsSwitchingOverProblem}
import js7.core.command.CommandMeta
import js7.core.problems.ReverseReleaseEventsProblem
import js7.data.Problems.{CannotDeleteChildOrderProblem, CannotDeleteWatchingOrderProblem, ClusterModuleShuttingDownProblem, UnknownOrderProblem}
import js7.data.agent.AgentRefStateEvent.{AgentEventsObserved, AgentMirroredEvent, AgentReady, AgentReset, AgentShutDown}
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRunId}
import js7.data.board.NoticeEvent.{NoticeDeleted, NoticePosted}
import js7.data.board.{BoardPath, NoticeEventSource, NoticeId, PlannedNoticeKey}
import js7.data.calendar.{Calendar, CalendarExecutor}
import js7.data.cluster.ClusterEvent
import js7.data.controller.ControllerCommand.{ChangePlanSchema, ControlWorkflow, ControlWorkflowPath, TransferOrders}
import js7.data.controller.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.data.controller.ControllerStateExecutor.convertImplicitly
import js7.data.controller.{ControllerCommand, ControllerEvent, ControllerEventColl, ControllerState, ControllerStatePlanFunctions, VerifiedUpdateItems, VerifiedUpdateItemsExecutor}
import js7.data.delegate.DelegateCouplingState
import js7.data.delegate.DelegateCouplingState.{Reset, Resetting}
import js7.data.event.JournalEvent.JournalEventsReleased
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, Stamped}
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.item.BasicItemEvent.{ItemAttached, ItemAttachedToMe, ItemDeleted, ItemDetached, ItemDetachingFromMe, SignedItemAttachedToMe}
import js7.data.item.ItemAttachedState.{Attachable, Detachable, Detached}
import js7.data.item.UnsignedItemEvent.{UnsignedItemAdded, UnsignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.item.{InventoryItem, InventoryItemEvent, InventoryItemKey, ItemAddedOrChanged, ItemRevision, SignableItemKey, UnsignedItem, UnsignedItemKey}
import js7.data.order.OrderEvent.{OrderActorEvent, OrderAdded, OrderAttachable, OrderAttached, OrderCancellationMarked, OrderCancellationMarkedOnAgent, OrderCoreEvent, OrderDeleted, OrderDetachable, OrderDetached, OrderGoes, OrderNoticePosted, OrderNoticePostedV2_3, OrderSuspensionMarked, OrderSuspensionMarkedOnAgent}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, OrderMark}
import js7.data.orderwatch.{OrderWatchEvent, OrderWatchPath, OrderWatchState}
import js7.data.plan.PlanSchemaEvent.PlanSchemaChanged
import js7.data.plan.{PlanId, PlanSchemaId, PlanSchemaState}
import js7.data.problems.UserIsNotEnabledToReleaseEventsProblem
import js7.data.state.OrderEventHandler
import js7.data.state.OrderEventHandler.FollowUp
import js7.data.subagent.SubagentItemStateEvent.{SubagentEventsObserved, SubagentResetStartedByController}
import js7.data.subagent.{SubagentBundle, SubagentId, SubagentItem, SubagentItemState, SubagentItemStateEvent}
import js7.data.value.expression.scopes.NowScope
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Instruction, Workflow, WorkflowControl, WorkflowControlId, WorkflowPathControl, WorkflowPathControlPath}
import js7.journal.state.FileJournal
import js7.journal.{CommitOptions, JournalActor, MainJournalingActor}
import org.apache.pekko.actor.{DeadLetterSuppression, Stash, Status, SupervisorStrategy, Terminated}
import org.apache.pekko.pattern.{ask, pipe}
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class ControllerOrderKeeper(
  stopped: Promise[ProgramTermination],
  journalAllocated: Allocated[IO, FileJournal[ControllerState]],
  clusterNode: WorkingClusterNode[ControllerState],
  alarmClock: AlarmClock,
  controllerConfiguration: ControllerConfiguration,
  testEventPublisher: EventPublisher[Any])
  (implicit protected val ioRuntime: IORuntime)
extends Stash, MainJournalingActor[ControllerState, Event]:

  import context.watch
  import controllerConfiguration.config
  import js7.controller.ControllerOrderKeeper.RichIdToOrder

  private given scheduler: Scheduler = ioRuntime.scheduler
  private given ExecutionContext = ioRuntime.compute

  override val supervisorStrategy: SupervisorStrategy = SupervisorStrategies.escalate
  private def journal = journalAllocated.allocatedThing
  protected def journalConf = controllerConfiguration.journalConf
  protected def journalActor = journal.journalActor

  private implicit val instructionExecutorService: InstructionExecutorService =
    new InstructionExecutorService(alarmClock)
  private val agentDriverConfiguration = AgentDriverConfiguration
    .fromConfig(config, controllerConfiguration.journalConf).orThrow
  private var _controllerState: ControllerState = ControllerState.Undefined

  private val agentRegister = mutable.Map[AgentPath, AgentEntry]()
  private val orderRegister = mutable.HashMap.empty[OrderId, OrderEntry]
  private val suppressOrderIdCheckFor = config
    .optionAs[String]("js7.TEST-ONLY.suppress-order-id-check-for")
  private val deleteOrderDelay = config.getDuration("js7.order.delete-delay").toFiniteDuration
  private val testAddOrderDelay = config
    .optionAs[FiniteDuration]("js7.TEST-ONLY.add-order-delay").fold(IO.unit)(IO.sleep)
  private var journalTerminated = false

  private object notices:
    private val noticeToSchedule = mutable.Map.empty[NoticeId, SyncCancelable]

    def maybeSchedule(noticeId: NoticeId, endOfLife: Option[Timestamp]): Unit =
      endOfLife.foreach:
        schedule(noticeId, _)

    def schedule(noticeId: NoticeId, endOfLife: Timestamp): Unit =
      noticeToSchedule += noticeId ->
        alarmClock.scheduleAt(endOfLife, s"NoticeIsDue($noticeId)"):
          self ! Internal.NoticeIsDue(noticeId)

    def deleteSchedule(noticeId: NoticeId): Unit =
      noticeToSchedule.remove(noticeId).foreach(_.cancel())

  private object shutdown:
    var delayUntil = scheduler.now()
    val since = SetOnce[SyncDeadline]
    private val shutDown = SetOnce[ControllerCommand.ShutDown]
    private val stillShuttingDownCancelable = SerialSyncCancelable()
    private var terminatingAgentDrivers = false
    private var takingSnapshot = false
    private var snapshotTaken = false
    private var terminatingJournal = false

    def shuttingDown = since.isDefined

    def restart = shutDown.toOption.fold(false)(_.restart)

    def start(shutDown: ControllerCommand.ShutDown): Unit =
      if !shuttingDown then
        since := scheduler.now()
        this.shutDown := shutDown
        stillShuttingDownCancelable := scheduler
          .scheduleAtFixedRates(controllerConfiguration.journalConf.ackWarnDurations/*?*/):
          self ! Internal.StillShuttingDown
        continue()

    def close(): Unit =
      stillShuttingDownCancelable.cancel()

    def onStillShuttingDown(): Unit =
      logger.info(s"Still shutting down, waiting for $runningAgentDriverCount AgentDrivers" +
        (!snapshotTaken ?? " and the snapshot"))

    def onSnapshotTaken(): Unit =
      if shuttingDown then
        snapshotTaken = true
        continue()

    def continue(): Unit =
      for shutDown <- shutDown do
        logger.trace(s"shutdown.continue: $runningAgentDriverCount AgentDrivers${
          !snapshotTaken ?? ", snapshot required"}")
        if !terminatingAgentDrivers then
          terminatingAgentDrivers = true
          agentRegister.values.map(_.agentDriver)
            .toVector
            .parTraverse(agentDriver =>
              agentDriver.terminate()
                .recoverWith(t => IO(logger.error(
                  s"$agentDriver.terminate => ${t.toStringWithCauses}", t.nullIfNoStackTrace)))
                .logWhenItTakesLonger(s"$agentDriver.terminate"))
            .unsafeRunAndForget() // TODO
        if runningAgentDriverCount == 0 then
          if !takingSnapshot then
            takingSnapshot = true
            if shutDown.suppressSnapshot then
              snapshotTaken = true
            else
              journalActor ! JournalActor.Input.TakeSnapshot
          if snapshotTaken && !terminatingJournal then
            // The event forces the cluster to acknowledge this event and the snapshot taken
            terminatingJournal = true
            persistKeyedEventIO(NoKey <-: ControllerShutDown)((_, _) => Completed)
              .flatTap(_ => journalAllocated.release)
              .unsafeToFuture()
              .onComplete:
                case Success(Right(Completed)) =>
                case other => logger.error(s"While shutting down: $other")
  import shutdown.shuttingDown

  /** Next orders to be processed. */
  private object orderQueue:
    private val queue = new VectorBuilder[OrderId]
    private val known = mutable.Set.empty[OrderId]
    private var notified = false

    def enqueue(orderIds: Iterable[OrderId]): Unit =
      if !shuttingDown && switchover.isEmpty && orderIds.nonEmpty then
        for orderId <- orderIds.iterator do
          if known.add(orderId) then
            queue += orderId
        if !notified then
          self ! Internal.ContinueWithNextOrderEvents
          notified = true

    def readAll(): Seq[OrderId] =
      notified = false
      val orderIds = queue.result()
      queue.clear()
      known.clear()
      orderIds

    override def toString = queue.result().map(_.string).mkString(", ")  // For debugging

  @volatile
  private var switchover: Option[Switchover] = None

  private final class Switchover(val restart: Boolean):
    // 1) Emit SwitchedOver event
    // 2) Terminate JournalActor
    // 3) Stop ControllerOrderKeeper includinge AgentDriver's
    // Do not terminate AgentDrivers properly because we do not want any events.

    private val stillSwitchingOverSchedule = scheduler
      .scheduleAtFixedRates(controllerConfiguration.journalConf.ackWarnDurations):
      logger.debug("Still switching over to the other cluster node")

    def start(): IO[Checked[Completed]] =
      clusterNode.switchOver   // Will terminate `cluster`, letting ControllerOrderKeeper terminate
        .flatMapT(o => journalAllocated.release.as(Right(o)))

    def close(): Unit = stillSwitchingOverSchedule.cancel()

  watch(journalActor)

  override def postStop(): Unit =
    try
      shutdown.close()
      switchover foreach { _.close() }
    finally
      logger.debug(
        "Stopped" + shutdown.since.toOption.fold("")(o => s" (terminated in ${o.elapsed.pretty})"))
      stopped.success(
        ProgramTermination(restart = switchover.exists(_.restart) | shutdown.restart))
      super.postStop()

  def receive: Receive =
    case Input.Start =>
      val controllerState = journal.unsafeCurrentState()
      if controllerState.controllerMetaState.isDefined then
        recover(controllerState)

      become("activating")(activating)
      unstashAll()
      clusterNode.beforeJournalingStarts
        .map(_.orThrow)
        .materialize
        .map(Internal.Activated.apply)
        .unsafeToFuture()
        .pipeTo(self)

    case msg => notYetReady(msg)

  private def recover(controllerState: ControllerState): Unit =
    if controllerState.controllerId != controllerConfiguration.controllerId then
      throw Problem(s"Recovered '${controllerState.controllerId}' " +
        s"differs from configured '${controllerConfiguration.controllerId}'"
      ).throwable
    this._controllerState = controllerState
    //controllerMetaState = controllerState.controllerMetaState.copy(totalRunningTime = recovered.totalRunningTime)

    controllerState.allNotices.foreach: notice =>
      notices.maybeSchedule(notice.id, notice.endOfLife)

    persistedEventId = controllerState.eventId

  private def activating: Receive =
    case Internal.Activated(Failure(t)) =>
      logger.error(s"Activation of this cluster node failed because: ${t.toStringWithCauses}")
      if t.getStackTrace.nonEmpty then logger.debug(t.toStringWithCauses, t)
      throw t.appendCurrentStackTrace

    case Internal.Activated(Success(())) =>
      // `become` must be called early, before any persist!
      become("becomingReady")(becomingReady)

      locally:
        val maybeControllerInitialized = !_controllerState.controllerMetaState.isDefined thenVector
          (NoKey <-: ControllerEvent.ControllerInitialized(
            controllerConfiguration.controllerId,
            journal.journalHeader.initiallyStartedAt))
        val controllerReady = NoKey <-: ControllerEvent.ControllerReady(
          Timezone(ZoneId.systemDefault.getId),
          totalRunningTime = journal.journalHeader.totalRunningTime)

        val events = maybeControllerInitialized :+
          controllerReady :++
          _controllerState.nextOrderWatchOrderEvents

        persistMultiple(events) { (_, updatedState) =>
          _controllerState = updatedState
          clusterNode.afterJournalingStarted
            .materializeIntoChecked
            .map(Internal.Ready.apply)
            .unsafeToFuture()
            .pipeTo(self)

          for path <- _controllerState.keyToItem(AgentRef).keys do
            proceedWithItem(path)
          for path <- _controllerState.keyTo(WorkflowPathControl).keys do
            proceedWithItem(path)
          for itemKey <- _controllerState.keyTo(WorkflowControl).keys do
            proceedWithItem(itemKey)
        }

      _controllerState.keyTo(OrderWatchState).keys foreach proceedWithItem

      // Proceed order before starting AgentDrivers, so AgentDrivers may match recovered OrderIds with Agent's OrderIds
      orderRegister ++= _controllerState.idToOrder.keys.map(_ -> new OrderEntry(scheduler.now()))

      // Start fetching events from Agents after AttachOrder has been sent to AgentDrivers.
      // This is to handle race-condition: An Agent may have already completed an order.
      // So send AttachOrder before DetachOrder.
      // The Agent will ignore the duplicate AttachOrder if it arrives before DetachOrder.
      for agentRef <- _controllerState.pathToUnsignedSimple(AgentRef).values do
        val agentRefState = _controllerState.keyTo(AgentRefState)
          .getOrElse(agentRef.path, AgentRefState(agentRef))
        registerAgent(agentRef, eventId = agentRefState.eventId)

      // Any ordering when continuing orders???
      proceedWithOrders(_controllerState.idToOrder.keys)
      orderQueue.enqueue(_controllerState.idToOrder.keys)

      if persistedEventId > EventId.BeforeFirst then // Recovered?
        logger.info(s"${_controllerState.idToOrder.size} Orders, " +
          s"${_controllerState.repo.typedCount[Workflow]} Workflows and " +
          s"${_controllerState.keyTo(AgentRefState).size} AgentRefs recovered")

    case Command.Execute(_: ControllerCommand.ShutDown, _, _) =>
      stash()

    case Command.Execute(cmd, _, correlId) =>
      correlId.bind:
        logger.warn(s"$ControllerIsNotReadyProblem: $cmd")
      sender() ! Left(ControllerIsNotReadyProblem)

    case cmd: Command =>
      logger.warn(s"$ControllerIsNotReadyProblem: $cmd")
      sender() ! Status.Failure(ControllerIsNotReadyProblem.throwable)

    case msg => notYetReady(msg)

  private def notYetReady(message: Any): Unit =
    message match
      case Command.Execute(_: ControllerCommand.ShutDown, _, _) =>
        stash()

      case Command.Execute(cmd, _, _) =>
        logger.warn(s"$ControllerIsNotReadyProblem: $cmd")
        sender() ! Left(ControllerIsNotReadyProblem)

      case Command.VerifiedUpdateItemsCmd(_) =>
        logger.warn(s"$ControllerIsNotReadyProblem: VerifiedUpdateItemsCmd")
        sender() ! Left(ControllerIsNotReadyProblem)

      case cmd: Command =>
        logger.warn(s"$ControllerIsNotReadyProblem: $cmd")
        sender() ! Status.Failure(ControllerIsNotReadyProblem.throwable)

      case _ => stash()

  private def becomingReady: Receive =
    case Internal.Ready(Left(problem)) =>
      logger.error(s"Appointment of configured cluster backup-node failed: $problem")
      throw problem.throwable.appendCurrentStackTrace

    case Internal.Ready(Right(Completed)) =>
      logger.info(ServiceMain.readyMessageWithLine(s"${_controllerState.controllerId} is ready"))
      testEventPublisher.publish(ControllerReadyTestIncident)
      clusterNode
        .onTerminatedUnexpectedly // Happens while isTest suppresses Halt after AckFromActiveClusterNode
        // Then we must stop ActiveClusterNode which may stick in ClusterWatch confirmation loop
        // (when ClusterWatch does not access this Controller)
        .<*(clusterNode.stop)
        .unsafeToFuture()
        .onComplete: tried =>
          self ! Internal.ClusterModuleTerminatedUnexpectedly(tried)
      become("Ready")(ready orElse handleExceptionalMessage)
      unstashAll()

    case _ =>
      // stash Command too, after ControllerReady event and cluster node has been initialized (see above)
      stash()

  private def ready: Receive =
    case Internal.ContinueWithNextOrderEvents =>
      val orderIds = orderQueue.readAll()
      val keyedEvents = nextOrderEvents(orderIds)
      if keyedEvents.nonEmpty then
        persistTransaction(keyedEvents)(handleEvents)

    case Command.Execute(command, meta, correlId) =>
      val sender = this.sender()
      if shuttingDown then
        sender ! Status.Success(Left(ControllerIsShuttingDownProblem))
      else if switchover.isDefined then
        sender ! Status.Success(Left(ControllerIsSwitchingOverProblem))
      else
        correlId.bind(
          executeControllerCommand(command, meta)
        ).onComplete:
          case Failure(t) => sender ! Status.Failure(t)
          case Success(response) => sender ! response

    case Command.VerifiedUpdateItemsCmd(verifiedUpdateItems: VerifiedUpdateItems) =>
      executeVerifiedUpdateItems(verifiedUpdateItems)

    case Command.GetClusterWatchService(agentPath) =>
      agentRegister.checked(agentPath)
        .traverse(_.agentDriver.clusterWatchService)
        .map(_.flatten)
        .unsafeToFuture()
        .pipeTo(sender())

    case Internal.EventsFromAgent(agentPath, agentRunId, stampedAgentEvents, committedPromise) =>
      for agentEntry <- agentRegister.get(agentPath) do
        for agentRefState <- journal.unsafeCurrentState().keyTo(AgentRefState).get(agentPath) do
          val isAgentReset = agentRefState.couplingState match
            case _: DelegateCouplingState.Resetting => true
            case DelegateCouplingState.Reset.byCommand => true
            case _ => false

          committedPromise.completeWith:
            if isAgentReset /*Race condition ???*/ then
              for o <- stampedAgentEvents.map(_.value) do logger.warn(
                s"Ignored event after Agent reset: $o")
              Future.successful(None)
            else if !agentRefState.agentRunId.forall(_ == agentRunId) then
              logger.debug(s"Internal.EventsFromAgent: Unknown agentRunId=$agentRunId")
              Future.successful(None)
            else
              var timestampedEvents: Seq[Timestamped[Event]] =
                stampedAgentEvents.view.flatMap {
                  case Stamped(_, timestampMillis, keyedEvent) =>
                    keyedEvent match
                      case KeyedEvent(orderId: OrderId, _: OrderCancellationMarked) =>
                        Timestamped(orderId <-: OrderCancellationMarkedOnAgent, Some(timestampMillis)) :: Nil

                      case KeyedEvent(orderId: OrderId, _: OrderSuspensionMarked) =>
                        Timestamped(orderId <-: OrderSuspensionMarkedOnAgent, Some(timestampMillis)) :: Nil

                      case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
                        val ownEvent = event match
                          case _: OrderEvent.OrderAttachedToAgent => OrderAttached(agentPath) // TODO Das kann schon der Agent machen. Dann wird weniger übertragen.
                          case _ => event
                        Timestamped(orderId <-: ownEvent, Some(timestampMillis)) :: Nil

                      case KeyedEvent(_: NoKey, AgentEvent.AgentReady(timezone, _, platformInfo)) =>
                        Timestamped(agentEntry.agentPath <-: AgentReady(timezone, platformInfo),
                          Some(timestampMillis)) :: Nil

                      case KeyedEvent(_: NoKey, AgentEvent.AgentShutDown) =>
                        Timestamped(
                          agentEntry.agentPath <-: AgentShutDown,
                          Some(timestampMillis)
                        ) :: Nil

                      case KeyedEvent(_: NoKey, ItemAttachedToMe(item)) =>
                        // COMPATIBLE with v2.1
                        Timestamped(NoKey <-: ItemAttached(item.key, item.itemRevision, agentPath)) :: Nil

                      case KeyedEvent(_: NoKey, SignedItemAttachedToMe(signed)) =>
                        // TODO Das kann schon der Agent machen. Dann wird weniger übertragen.
                        val item = signed.value
                        Timestamped(NoKey <-: ItemAttached(item.key, item.itemRevision, agentPath)) :: Nil

                      case KeyedEvent(_: NoKey, _: ItemDetachingFromMe) =>
                        Nil

                      case KeyedEvent(_: NoKey, _: ItemDetached) =>
                        Timestamped(keyedEvent) :: Nil

                      case KeyedEvent(_: OrderWatchPath, _: OrderWatchEvent) =>
                        Timestamped(keyedEvent) :: Nil

                      case KeyedEvent(_, _: ItemAddedOrChanged) =>
                        Nil

                      case KeyedEvent(_: SubagentId, event: SubagentItemStateEvent) =>
                        event match
                          case _: SubagentEventsObserved => Nil // Not needed
                          case _ => Timestamped(keyedEvent) :: Nil

                      case ke @ KeyedEvent(_: NoKey, _: ClusterEvent) =>
                        Timestamped(agentPath <-: AgentMirroredEvent(ke)) :: Nil

                      case _ =>
                        logger.error(s"Unknown event received from ${agentEntry.agentPath}: $keyedEvent")
                        Nil
                }.toVector

              if timestampedEvents.isEmpty then
                // timestampedEvents may be empty if it contains only discarded (Agent-only) events.
                // Agent's last observed EventId is not persisted then, and we do not write an AgentEventsObserved.
                // For tests, this makes the journal predictable after OrderFinished (because no AgentEventsObserved may follow).
                Future.successful(None)
              else
                val agentEventId = stampedAgentEvents.last.eventId
                timestampedEvents :+= Timestamped(agentPath <-: AgentEventsObserved(agentEventId))

                val subseqEvents = subsequentEvents(timestampedEvents.map(_.keyedEvent))
                orderQueue.enqueue(
                  subseqEvents.view.collect { case KeyedEvent(orderId: OrderId, _) => orderId }) // For OrderSourceEvents
                timestampedEvents ++= subseqEvents.map(Timestamped(_))

                journal.unsafeCurrentState().keyTo(AgentRefState).get(agentPath).map(_.couplingState) match
                  case Some(DelegateCouplingState.Resetting(_) | DelegateCouplingState.Reset(_)) =>
                    // Ignore the events, because orders are already marked as detached (and Failed)
                    // TODO Avoid race-condition and guard with journal.lock!
                    // (switch from actors to IO required!)
                    Future.successful(None)
                  case _ =>
                    persistTransactionTimestamped(timestampedEvents,
                      CommitOptions(alreadyDelayed = agentDriverConfiguration.eventBufferDelay)):
                      (stampedEvents, updatedState) =>
                        handleEvents(stampedEvents, updatedState)
                        Some(agentEventId)

    case Internal.OrdersMarked(orderToMark) =>
      // TODO Maybe execute this code when the corresponding event arrives:
      // Like already OrderGoMarked: OrderSuspensionMarked, OrderResumptionMarked, ...
      // Then we must not handle a late AgentCommand.MarkOrder response.
      val unknown = orderToMark -- _controllerState.idToOrder.keySet
      if unknown.nonEmpty then
        logger.error("Response to AgentCommand.MarkOrder from Agent for unknown orders: " +
          unknown.mkString(", "))
      orderToMark.foreach:
        case (orderId, _: OrderMark.Go) =>
          // Do not set agentOrderMark. This would defeat OrderGoes, which clears agentOrderMark.
          // OrderGoes event may arrive before MarkOrders response.
        case (orderId, mark) =>
          orderRegister(orderId).agentOrderMark = Some(mark)

    case JournalActor.Output.SnapshotTaken =>
      shutdown.onSnapshotTaken()

    case Internal.OrderIsDue(orderId) =>
      proceedWithOrders(orderId :: Nil)
      orderQueue.enqueue(orderId :: Nil)

    case Internal.NoticeIsDue(noticeId) =>
      notices.deleteSchedule(noticeId)
      for
        plan <- _controllerState.toPlan.get(noticeId.planId);
        plannedBoard <- plan.toPlannedBoard.get(noticeId.boardPath)
        notice <- plannedBoard.maybeNotice(noticeId.noticeKey)
        keyedEvent <- plannedBoard.deleteNoticeEvent(noticeId.noticeKey).toOption
      do
        if notice.endOfLife.exists(alarmClock.now() < _) then
          notices.maybeSchedule(noticeId, notice.endOfLife)
        else
          logger.debug(s"Notice lifetime expired: $noticeId")
          persistMultiple(keyedEvent :: Nil)(handleEvents)

    case Internal.ShutDown(shutDown) =>
      shutdown.delayUntil = scheduler.now() + config.getDuration("js7.web.server.delay-shutdown")
        .toFiniteDuration
      shutdown.start(shutDown)

    case Internal.StillShuttingDown =>
      shutdown.onStillShuttingDown()

    case Internal.AgentDriverStopped(agentPath) if agentRegister contains agentPath =>
      var agentEntry = agentRegister(agentPath)
      agentEntry.actorTerminated = true
      agentEntry.release.unsafeRunAndForget()/*???*/ // Release in case there are surrounding Resources
      if switchover.isDefined && journalTerminated && runningAgentDriverCount == 0 then
        val delay = shutdown.delayUntil.timeLeft
        if delay.isPositive then
          logger.debug(s"Sleep ${delay.pretty} after ShutDown command")
          sleep(delay)
        context.stop(self)
      else if shuttingDown then
        shutdown.continue()
      else
        agentRegister -= agentPath
        for agentRefState <- journal.unsafeCurrentState().keyTo(AgentRefState).checked(agentPath) do
          agentRefState.couplingState match
            case Resetting(_) | Reset(_) =>
              agentEntry = registerAgent(agentRefState.agentRef, eventId = EventId.BeforeFirst)
            //??? reattachToAgent(agentPath)

            case _ =>
              logger.debug(s"AgentDriver for $agentPath terminated")

  private def executeVerifiedUpdateItems(verifiedUpdateItems: VerifiedUpdateItems): Unit =
    val t = scheduler.now()
    (for
      keyedEvents <- VerifiedUpdateItemsExecutor.execute(verifiedUpdateItems, _controllerState, {
        case calendar: Calendar =>
          CalendarExecutor.checked(calendar, Timezone.utc/*irrelevant*/).rightAs(())
      })
      _ <- checkAgentDriversAreTerminated(
        keyedEvents.view
          .collect { case KeyedEvent(_, UnsignedSimpleItemAdded(a: AgentRef)) => a.path })
    yield keyedEvents)
    match
      case Left(problem) =>
        sender() ! Left(problem)

      case Right(keyedEvents) =>
        val sender = this.sender()
        persistTransactionAndSubsequentEvents(keyedEvents)(handleEvents)
          .map(_ => Right(Completed))
          .map(_.map { o =>
            if t.elapsed > 1.s then logger.debug("VerifiedUpdateItemsCmd - " +
              itemsPerSecondString(t.elapsed, verifiedUpdateItems.itemCount, "items"))
            o
          })
          .onComplete:
            case Failure(t) => sender ! Status.Failure(t)
            case Success(response) => sender ! (response: Checked[Completed])

  private def checkAgentDriversAreTerminated(addedAgentPaths: Iterable[AgentPath])
  : Checked[Unit] =
    val runningAgentDrivers = addedAgentPaths.filter(agentRegister.contains)
    if runningAgentDrivers.nonEmpty then
      Left(Problem("AgentDrivers for the following Agents are still running — " +
        s"please retry after some seconds: ${runningAgentDrivers.map(_.string).mkString(", ")}"))
    else
      Checked.unit

  // JournalActor's termination must be handled in any `become`-state and
  // must lead to ControllerOrderKeeper's termination
  override def journaling: Receive =
    handleExceptionalMessage orElse super.journaling

  private def handleExceptionalMessage: Receive =
    case Terminated(actor) if actor == journalActor =>
      journalTerminated = true
      if !shuttingDown && switchover.isEmpty then logger.error("JournalActor terminated")
      if switchover.isDefined && runningAgentDriverCount > 0 then
        agentRegister.values.map(_.agentDriver) foreach { agentDriver =>
          agentDriver
            .terminate(noJournal = true)
            .recoverWith(t => IO(logger.error(
              s"$agentDriver.terminate => ${t.toStringWithCauses}", t.nullIfNoStackTrace)))
            .logWhenItTakesLonger(s"$agentDriver.terminate")
            .unsafeRunAndForget() // TODO
        }
      else
        context.stop(self)

    case Internal.ClusterModuleTerminatedUnexpectedly(tried) =>
      // Stacktrace has been debug-logged by Cluster
      tried match
        case Success(Left(problem @ ClusterModuleShuttingDownProblem)) if shuttingDown =>
          logger.debug(s"Cluster module terminated with $problem")
        case Success(checked: Checked[Completed]) =>
          val msg: Any = checked.fold(identity, identity)
          logger.error(s"Cluster module terminated unexpectedly: $msg")
        case Failure(t) =>
          logger.error(s"Cluster module terminated unexpectedly: ${t.toStringWithCauses}", t)
      context.stop(self)

  private def executeControllerCommand(command: ControllerCommand, commandMeta: CommandMeta)
  : Future[Checked[ControllerCommand.Response]] =
    command match
      case ControllerCommand.AddOrder(order) =>
        if shuttingDown then
          Future.successful(Left(ControllerIsShuttingDownProblem))
        else if switchover.isDefined then
          Future.successful(Left(ControllerIsSwitchingOverProblem))
        else
          addOrder(order)
            .map(_.map(added => ControllerCommand.AddOrder.Response(ignoredBecauseDuplicate = !added)))

      case ControllerCommand.AddOrders(orders) =>
        if shuttingDown then
          Future.successful(Left(ControllerIsShuttingDownProblem))
        else if switchover.isDefined then
          Future.successful(Left(ControllerIsSwitchingOverProblem))
        else
          addOrders(orders).map(_.map(eventId =>
            ControllerCommand.AddOrders.Response(eventId)))

      case ControllerCommand.CancelOrders(orderIds, mode) =>
        executeOrderMarkCommands(orderIds.toVector):
          orderEventSource.cancel(_, mode)

      case ControllerCommand.SuspendOrders(orderIds, mode) =>
        executeOrderMarkCommands(orderIds.toVector):
          orderEventSource.suspend(_, mode)

      case ControllerCommand.GoOrder(orderId, position) =>
        executeOrderMarkCommands(Vector(orderId)):
          orderEventSource.go(_, position)

      case ControllerCommand.ResumeOrder(orderId, position, historicOps, asSucceeded, restartJob) =>
        executeOrderMarkCommands(Vector(orderId)):
          orderEventSource.resume(_, position, historicOps, asSucceeded, restartJob)

      case cmd: ControllerCommand.TransferOrders =>
        executeTransferOrders(cmd)

      case cmd: ControllerCommand.ChangePlanSchema =>
        changePlanSchema(cmd)

      case cmd: ControllerCommand.ControlWorkflowPath =>
        controlWorkflowPath(cmd)

      case cmd: ControllerCommand.ControlWorkflow =>
        controlWorkflow(cmd)

      case ControllerCommand.ResumeOrders(orderIds, asSucceeded, restartKilledJob) =>
        executeOrderMarkCommands(orderIds.toVector):
          orderEventSource.resume(_, None, Nil, asSucceeded, restartKilledJob)

      case cmd: ControllerCommand.PostNotice =>
        NoticeEventSource(alarmClock).executePostNoticeCommand(cmd, _controllerState) match
          case Left(problem) => Future.successful(Left(problem))
          case Right(events) =>
            persistTransactionAndSubsequentEvents(events)(handleEvents)
              .map(_ => Right(ControllerCommand.Response.Accepted))

      case ControllerCommand.DeleteNotice(noticeId) =>
        (for
          plan <- _controllerState.toPlan.checked(noticeId.planId);
          plannedBoard <- plan.toPlannedBoard.checked(noticeId.boardPath)
          notice <- plannedBoard.checkedNotice(noticeId.noticeKey)
          keyedEvent <- plannedBoard.deleteNoticeEvent(noticeId.noticeKey)
        yield keyedEvent)
        match
          case Left(problem) => Future.successful(Left(problem))
          case Right(keyedEvent) =>
            persistTransactionAndSubsequentEvents(keyedEvent :: Nil)(handleEvents)
             .map(_ => Right(ControllerCommand.Response.Accepted))

      case cmd: ControllerCommand.ChangeGlobalToPlannableBoard =>
        import cmd.{planSchemaId, plannableBoard}
        ControllerStatePlanFunctions
          .changeBoardType(
            plannableBoard,
            fromPlanSchemaId = PlanSchemaId.Global,
            toPlanSchemaId = planSchemaId,
            endOfLife = None,
            _controllerState
          ):
            plannedNoticeKey =>
              cmd.evalSplitNoticeKey(plannedNoticeKey.noticeKey).map: (planKey, noticeKey) =>
                Some(planSchemaId / planKey / noticeKey)
        match
          case Left(problem) => Future.successful(Left(problem))
          case Right(keyedEvents) =>
            persistTransactionAndSubsequentEvents(keyedEvents)(handleEvents)
              .map(_ => Right(ControllerCommand.Response.Accepted))

      case cmd: ControllerCommand.ChangePlannableToGlobalBoard =>
        import cmd.{globalBoard, planSchemaId}
        globalBoard.evalEndOfLife(NowScope(alarmClock.now())).flatMap: endOfLife =>
          ControllerStatePlanFunctions.changeBoardType(
            globalBoard,
            fromPlanSchemaId = planSchemaId,
            toPlanSchemaId = PlanSchemaId.Global,
            endOfLife,
            _controllerState
          ):
            case PlannedNoticeKey(PlanId(`planSchemaId`, planKey), noticeKey) =>
              cmd.evalMakeNoticeKey(planKey, noticeKey).map: noticeKey =>
                Some(PlanId.Global / noticeKey)
            case _ => Right(None) // Alien planSchemaId
        match
          case Left(problem) => Future.successful(Left(problem))
          case Right(keyedEvents) =>
            persistTransactionAndSubsequentEvents(keyedEvents)(handleEvents)
              .map(_ => Right(ControllerCommand.Response.Accepted))

      case ControllerCommand.DeleteOrdersWhenTerminated(orderIds) =>
        orderIds.toVector
          .traverse(_controllerState.idToOrder.checked)
          .traverse(orders =>
            orders.traverse(order =>
              if order.parent.isDefined then
                Left(CannotDeleteChildOrderProblem(order.id): Problem)
              else if order.hasNonVanishedExternalOrder then
                Left(CannotDeleteWatchingOrderProblem(order.id): Problem)
              else
                Right(order)))
          .flatten
          .traverse(_
            .filterNot(_.deleteWhenTerminated)
            .traverse(orderEventSource.orderDeletedEvent))
          .flatten
          .map(_.flatten)
          .traverse(keyedEvents =>
            persistTransactionAndSubsequentEvents(keyedEvents)(handleEvents)
              .map(_ => ControllerCommand.Response.Accepted))

      case ControllerCommand.ReleaseEvents(untilEventId) =>
        val userId = commandMeta.user.id
        if !controllerConfiguration.journalConf.releaseEventsUserIds.contains(userId) then
          Future(Left(UserIsNotEnabledToReleaseEventsProblem))
        else
          val current = _controllerState.journalState.userIdToReleasedEventId.getOrElse(userId, EventId.BeforeFirst)
          if untilEventId < current then
            Future(Left(ReverseReleaseEventsProblem(requestedUntilEventId = untilEventId, currentUntilEventId = current)))
          else
            persist(JournalEventsReleased(userId, untilEventId)) { (_, updatedState) =>
              _controllerState = updatedState
              Right(ControllerCommand.Response.Accepted)
            }

      case ControllerCommand.NoOperation(maybeDuration) =>
        // NoOperation completes only after ControllerOrderKeeper has become ready
        // (can be used to await readiness)
        IO.pure(Right(ControllerCommand.Response.Accepted))
          .delayBy(maybeDuration getOrElse 0.s)
          .unsafeToFuture()

      case _: ControllerCommand.EmergencyStop | _: ControllerCommand.Batch =>
        // For completeness. RunningController has handled the command already
        Future.successful(Left(Problem.pure("THIS SHOULD NOT HAPPEN")))  // Never called

      case ControllerCommand.TakeSnapshot =>
        import controllerConfiguration.implicitPekkoAskTimeout  // We need several seconds or even minutes
        intelliJuseImport(implicitPekkoAskTimeout)
        (journalActor ? JournalActor.Input.TakeSnapshot)
          .mapTo[JournalActor.Output.SnapshotTaken.type]
          .map(_ => Right(ControllerCommand.Response.Accepted))

      case ControllerCommand.ClusterSwitchOver(None) =>
        clusterSwitchOver(restart = true)

      case shutDown: ControllerCommand.ShutDown =>
        shutDown.clusterAction match
          case Some(ControllerCommand.ShutDown.ClusterAction.Switchover) =>
            clusterSwitchOver(restart = shutDown.restart)

          case Some(ControllerCommand.ShutDown.ClusterAction.Failover) =>
            // TODO ClusterState.Coupled !
            shutdown.start(shutDown)
            Future.successful(Right(ControllerCommand.Response.Accepted))

          case None =>
            clusterNode.shutDownThisNode
              .flatTap:
                case Right(Completed) => IO { self ! Internal.ShutDown(shutDown) }
                case _ => IO.unit
              .map(_.map((_: Completed) => ControllerCommand.Response.Accepted))
              .unsafeToFuture()

      case ControllerCommand.EmitTestEvent =>
        persist(ControllerTestEvent, async = true) { (_, updatedState) =>
          _controllerState = updatedState
          Right(ControllerCommand.Response.Accepted)
        }

      case ControllerCommand.ResetAgent(agentPath, force) =>
        agentRegister.checked(agentPath) match
          case Left(problem) => Future.successful(Left(problem))
          case Right(agentEntry) =>
            journal.unsafeCurrentState().keyTo(AgentRefState).checked(agentEntry.agentPath) match
              case Left(problem) => Future.successful(Left(problem))
              case Right(agentRefState) =>
                // TODO journal.lock(agentPath), to avoid race with AgentCoupled, too
                // As a workaround, AgentRefState.applyEvent ignores AgentCoupled if Resetting
                agentRefState.couplingState match
                  case Resetting(frc) if !force || frc != force =>
                    Future.successful(Left(Problem.pure("AgentRef is already in state 'Resetting'")))
                  case reset: Reset if !force =>
                    Future.successful(Left(Problem.pure(s"AgentRef is already in state '$reset'")))
                  case _ =>
                    journal.unsafeCurrentState().resetAgent(agentPath, force = force) match
                      case Left(problem) => Future.successful(Left(problem))
                      case Right(events) =>
                        journal.unsafeCurrentState().applyKeyedEvents(events) match
                          case Left(problem) => Future.successful(Left(problem))
                          case Right(_) =>
                            persistTransactionAndSubsequentEvents(events) { (stampedEvents, updatedState) =>
                              // ResetAgent command may return with error despite it has reset the orders
                              agentEntry.isResetting = true
                              handleEvents(stampedEvents, updatedState)
                              agentEntry
                                .agentDriver.reset(force = force)
                                .onError(t => IO(
                                  logger.error("ResetAgent: " + t.toStringWithCauses, t)))
                                .materializeIntoChecked
                                .flatMapT(_ =>
                                  journal.persist(_
                                    .keyTo(AgentRefState).checked(agentPath)
                                    .map(_.couplingState)
                                    .map {
                                      case Resetting(_) => (agentPath <-: AgentReset) :: Nil
                                      case _ => Nil
                                    }))
                                .rightAs(ControllerCommand.Response.Accepted)
                                .unsafeToFuture()
                            }.flatten

      case ControllerCommand.ResetSubagent(subagentId, force) =>
        val controllerState = journal.unsafeCurrentState()
        controllerState.keyTo(SubagentItemState).checked(subagentId)
          .flatTap(subagentItemState =>
            controllerState.keyToItem(AgentRef).checked(subagentItemState.subagentItem.agentPath)
              .flatTap(agentRef =>
                // We double-check this here, then we can immediately return the problem.
                // When the Director rejects ResetAgent, we have no mean to return the rejection
                // to the command issuer.
                !agentRef.directors.contains(subagentId) !!
                  Problem.pure(s"$subagentId as a Agent Director cannot be reset")))
          .map(subagentItemState =>
            (subagentItemState.couplingState != DelegateCouplingState.Resetting(force))
              .thenList((subagentId <-: SubagentResetStartedByController(force = force))))
          .match
            case Left(problem) => Future.successful(Left(problem))
            case Right(events) =>
              persistMultiple(events) { (_, updated) =>
                _controllerState = updated
                proceedWithItem(subagentId)
              }.map(_ => Right(ControllerCommand.Response.Accepted))

      case ControllerCommand.AnswerOrderPrompt(orderId) =>
        orderEventSource.answerPrompt(orderId) match
          case Left(problem) =>
            Future.successful(Left(problem))
          case Right(events) =>
            persistTransactionAndSubsequentEvents(events)(handleEvents)
              .map(_ => Right(ControllerCommand.Response.Accepted))

      case ControllerCommand.ClusterSwitchOver(Some(agentPath)) =>
        agentRegister.checked(agentPath)
          .map(_.agentDriver)
          .match
            case Left(problem) => Future.successful(Left(problem))
            case Right(agentDriver) =>
              agentDriver
                .executeCommandDirectly(AgentCommand.ClusterSwitchOver)
                .logWhenItTakesLonger(s"$agentDriver.send(ClusterSwitchOver)")
                .materializeIntoChecked
                .rightAs(ControllerCommand.Response.Accepted)
                .unsafeToFuture()

      case ControllerCommand.ConfirmClusterNodeLoss(agentPath, lostNodeId, confirmer) =>
        agentRegister.checked(agentPath)
          .map(_.agentDriver)
          .match
            case Left(problem) => Future.successful(Left(problem))
            case Right(agentDriver) =>
              agentDriver
                .confirmClusterNodeLoss(lostNodeId, confirmer)
                .rightAs(ControllerCommand.Response.Accepted)
                .unsafeToFuture()

      case _ =>
        // Handled by ControllerCommandExecutor
        Future.failed(new NotImplementedError)

  private def executeOrderMarkCommands(orderIds: Vector[OrderId])
    (toEvents: OrderId => Checked[List[OrderActorEvent]])
  : Future[Checked[ControllerCommand.Response]] =
    if !orderIds.areUnique then
      Future.successful(Left(Problem.pure("OrderIds must be unique")))
    else
      orderIds.traverse(_controllerState.idToOrder.checked) match
        case Left(problem) =>
          Future.successful(Left(problem))

        case Right(orders) =>
          orders
            .flatTraverse: order =>
              toEvents(order.id)
                .map(_.toVector.map(order.id <-: _))
            .traverse: keyedEvents =>
              // Event may be inserted between events coming from Agent
              persistTransactionAndSubsequentEvents(keyedEvents)(handleEvents)
            .map(_.map(_ => ControllerCommand.Response.Accepted))

  private def executeTransferOrders(cmd: TransferOrders)
  : Future[Checked[ControllerCommand.Response]] =
    new TransferOrderEventSource(_controllerState)
      .transferOrders(cmd)
      .match
        case Left(problem) => Future.successful(Left(problem))
        case Right(events) =>
          persistTransaction(events ++ subsequentEvents(events)) { (stamped, updatedState) =>
            handleEvents(stamped, updatedState)
            Right(ControllerCommand.Response.Accepted)
          }

  private def changePlanSchema(cmd: ChangePlanSchema)
  : Future[Checked[ControllerCommand.Response]] =
    val planSchemaId = cmd.planSchemaId
    if planSchemaId.isGlobal then
      Future.successful(Left(Problem("Global PlanSchema cannot be changed")))
    else
      ControllerEventColl.keyedEvents[PlanSchemaChanged | NoticeDeleted](_controllerState): coll =>
        for
          coll <- coll.add:
            planSchemaId <-: PlanSchemaChanged(namedValues = cmd.namedValues)
          planSchemaState <-
            coll.aggregate.keyTo(PlanSchemaState).checked(planSchemaId)
          coll <- coll.add:
            planSchemaState.planIds.view.flatMap: planKey =>
              coll.aggregate.deadPlanNoticeDeleted(planSchemaId / planKey)
        yield
          coll
      match
        case Left(problem) => Future.successful(Left(problem))
        case Right(keyedEvents) =>
          persistTransactionAndSubsequentEvents(keyedEvents): (stamped, updated) =>
            handleEvents(stamped, updated)
            Right(ControllerCommand.Response.Accepted)

  private def controlWorkflowPath(cmd: ControlWorkflowPath)
  : Future[Checked[ControllerCommand.Response]] =
    _controllerState.repo.pathToItems(Workflow).checked(cmd.workflowPath) match
      case Left(problem) => Future.successful(Left(problem))
      case Right(_) =>
        val path = WorkflowPathControlPath(cmd.workflowPath)
        val (itemState, isNew) = _controllerState
          .pathToUnsignedSimple(WorkflowPathControl)
          .get(path) match
            case None => WorkflowPathControl(path) -> true
            case Some(o) => o -> false
        var item = itemState.item
        item = item.incrementRevision.copy(
          suspended = cmd.suspend.fold(item.suspended)(identity),
          skip = item.skip
            -- cmd.skip.filterNot(_._2).keys
            ++ cmd.skip.filter(_._2).keys)
        val event = if isNew then UnsignedSimpleItemAdded(item) else UnsignedSimpleItemChanged(item)

        val keyedEvents = Vector(event)
          .concat(_controllerState.updatedWorkflowPathControlAttachedEvents(item))
          .map(NoKey <-: _)

        // Continue even if WorkflowPathControl is not changed.
        // This allows the caller to force the redistribution of the WorkflowPathControl.
        persistTransactionAndSubsequentEvents(keyedEvents) { (stamped, updated) =>
          handleEvents(stamped, updated)
          proceedWithItem(path)
          val workflowPathControl = updated.keyTo(WorkflowPathControl)(path)
          if !workflowPathControl.item.suspended then
            orderQueue.enqueue(
              updated.orders.filter(_.workflowPath == workflowPathControl.workflowPath).map(_.id))
          Right(ControllerCommand.Response.Accepted)
        }

  private def controlWorkflow(cmd: ControlWorkflow)
  : Future[Checked[ControllerCommand.Response]] =
    _controllerState.repo.idTo(Workflow)(cmd.workflowId) match
      case Left(problem) => Future.successful(Left(problem))
      case Right(_) =>
        val workflowControlId = WorkflowControlId(cmd.workflowId)
        val (item0, isNew) = _controllerState
          .keyTo(WorkflowControl)
          .get(workflowControlId) match
            case None => WorkflowControl(workflowControlId) -> true
            case Some(o) => o -> false
        val item = item0.nextRevision.copy(
          breakpoints = item0.breakpoints -- cmd.removeBreakpoints ++ cmd.addBreakpoints)

        val event = if isNew then UnsignedItemAdded(item) else UnsignedItemChanged(item)
        val keyedEvents = Vector(event)
          .concat(_controllerState.updatedWorkflowControlAttachedEvents(item))
          .map(NoKey <-: _)

        persistTransactionAndSubsequentEvents(keyedEvents) { (stamped, updated) =>
          handleEvents(stamped, updated)
          proceedWithItem(workflowControlId)
          Right(ControllerCommand.Response.Accepted)
        }

  private def registerAgent(agent: AgentRef, eventId: EventId): AgentEntry =
    val allocated = AgentDriver
      .resource(agent, eventId = eventId,
        (agentRunId, events) => IO.defer {
          val promise = Promise[Option[EventId]]()
          self ! Internal.EventsFromAgent(agent.path, agentRunId, events, promise)
          IO.fromFuture(IO.pure(promise.future))
        },
        orderIdToMarked => IO {
          self ! Internal.OrdersMarked(orderIdToMarked)
          // TODO Asynchronous ?
        },
        journal, agentDriverConfiguration, controllerConfiguration, context.system)
      .toAllocated
      .logWhenItTakesLonger("registerAgent")
      .awaitInfinite // TODO Blocking

    allocated.allocatedThing.untilStopped
      .*>(IO(
        self ! Internal.AgentDriverStopped(agent.path)))
      .unsafeRunAndForget() // TODO

    val entry = AgentEntry(agent, allocated)
    agentRegister.insert(agent.path, entry)
    entry

  //private def reattachToAgent(agentPath: AgentPath): Unit =
  //  for (actor <- agentRegister.get(agentPath).map(_.actor)) {
  //    for ((itemKey, agentToAttachedState) <- _controllerState.itemToAgentToAttachedState) {
  //      agentToAttachedState.get(agentPath) foreach {
  //        case Attachable =>
  //          itemKey match {
  //            case itemKey: SignableItemKey =>
  //              for (signedItem <- _controllerState.keyToSignedItem.get(itemKey)) {
  //                actor ! AgentDriver.Queueable.AttachSignedItem(signedItem)
  //              }
  //
  //            case path: UnsignedSimpleItemPath =>
  //              for (item <- _controllerState.pathToUnsignedSimpleItem.get(path)) {
  //                actor ! AgentDriver.Queueable.AttachUnsignedItem(item)
  //              }
  //          }
  //
  //        case Detachable =>
  //          actor ! AgentDriver.Queueable.DetachItem(itemKey)
  //
  //        case _ =>
  //      }
  //    }
  //
  //    _controllerState.idToOrder.valuesIterator
  //      .filter(_.attachedState.contains(Order.Attaching(agentPath)))
  //      .flatMap(_.checkedState[Order.IsFreshOrReady].toOption)
  //      .foreach(tryAttachOrderToAgent)
  //  }

  private def addOrder(freshOrder: FreshOrder): Future[Checked[Boolean]] =
    _controllerState.addOrder(freshOrder, suppressOrderIdCheckFor = suppressOrderIdCheckFor)
    match
      case Left(problem) => Future.successful(Left(problem))
      case Right(Left(existing)) =>
        logger.debug(s"Discarding duplicate added Order: $freshOrder")
        Future.successful(Right(false))

      case Right(Right(orderAddedEvents)) =>
        val events = orderAddedEvents.toKeyedEvents
        persistTransactionAndSubsequentEvents(events): (stamped, updatedState) =>
          handleEvents(stamped, updatedState)
          Right(true)
        .flatMap: o =>
          testAddOrderDelay.unsafeToFuture().map(_ => o) // test only

  private def addOrders(freshOrders: Seq[FreshOrder]): Future[Checked[EventId]] =
    _controllerState.addOrders(freshOrders, suppressOrderIdCheckFor = suppressOrderIdCheckFor)
    match
      case Left(problem) => Future.successful(Left(problem))
      case Right(events) =>
        persistTransaction(events) { (stamped, updatedState) =>
          handleEvents(stamped, updatedState)
          // Emit subsequent events later for earlier addOrders response (and smaller event chunk)
          orderQueue.enqueue(freshOrders.view.map(_.id))
          Right(updatedState.eventId)
        }

  private def persistTransactionAndSubsequentEvents[A](keyedEvents: Seq[KeyedEvent[Event]])
    (callback: (Seq[Stamped[KeyedEvent[Event]]], ControllerState) => A)
  : Future[A] =
    persistTransaction(keyedEvents ++ subsequentEvents(keyedEvents))(callback)

  private def subsequentEvents(keyedEvents: Seq[KeyedEvent[Event]]): Seq[KeyedEvent[Event]] =
    delayOrderDeletion(
      _controllerState
        .applyEventsAndReturnSubsequentEvents(keyedEvents)
        .map(_.keyedEvents)
        .orThrow)

  private def nextOrderEvents(orderIds: Seq[OrderId]): Seq[AnyKeyedEvent] =
    delayOrderDeletion(
      _controllerState.nextOrderEvents(orderIds).keyedEvents)

  private def handleEvents(
    stampedEvents: Seq[Stamped[KeyedEvent[Event]]],
    updatedState: ControllerState)
  : Unit =
    val itemKeys = mutable.Set.empty[InventoryItemKey]
    val orderIds = mutable.Set.empty[OrderId]
    for stamped <- stampedEvents do
      val keyedEvent = stamped.value
      keyedEvent match
        case KeyedEvent(orderId: OrderId, _: OrderEvent) =>
          orderIds += orderId
          orderIds ++= handleOrderEvent(keyedEvent.asInstanceOf[KeyedEvent[OrderEvent]])
          _controllerState = _controllerState.applyKeyedEvents(keyedEvent :: Nil).orThrow

        case KeyedEvent(_: NoKey, event: InventoryItemEvent) =>
          _controllerState = _controllerState.applyKeyedEvents(keyedEvent :: Nil).orThrow
          itemKeys += event.key
          handleItemEvent(event)

        case KeyedEvent(boardPath: BoardPath, noticePosted: NoticePosted) =>
          val noticeId = boardPath / noticePosted.plannedNoticeKey
          notices.deleteSchedule(noticeId)
          notices.maybeSchedule(noticeId, noticePosted.endOfLife)
          _controllerState = _controllerState.applyKeyedEvents(keyedEvent :: Nil).orThrow

        case KeyedEvent(boardPath: BoardPath, NoticeDeleted(plannedNoticeKey)) =>
          notices.deleteSchedule(boardPath / plannedNoticeKey)
          _controllerState = _controllerState.applyKeyedEvents(keyedEvent :: Nil).orThrow

        case _ =>
          _controllerState = _controllerState.applyKeyedEvents(keyedEvent :: Nil).orThrow
    _controllerState = updatedState  // Reduce memory usage (they are equal)
    itemKeys foreach proceedWithItem
    proceedWithOrders(orderIds)

  private def handleItemEvent(event: InventoryItemEvent): Unit =
    event match
      case UnsignedSimpleItemAdded(agentRef: AgentRef) =>
        registerAgent(agentRef, eventId = EventId.BeforeFirst)

        // TODO Not required in a future implementation, when Agents must be defined when referenced
        //reattachToAgent(agentRef.path)

      case UnsignedSimpleItemChanged(agentRef: AgentRef) =>
        agentRegister(agentRef.path) = agentRegister(agentRef.path).copy(agentRef = agentRef)
        val agentDriver = agentRegister(agentRef.path).agentDriver
        agentDriver
          .changeAgentRef(agentRef)
          .handleErrorWith(t => IO(logger.error(
            s"$agentDriver.changeAgentRef => ${t.toStringWithCauses}", t.nullIfNoStackTrace)))
          .logWhenItTakesLonger(s"$agentDriver.changeAgentRef")
          .unsafeRunAndForget() // TODO
          //.awaitInfinite until v2.7

      case UnsignedSimpleItemChanged(subagentItem: SubagentItem) =>
        for agentRef <- journal.unsafeCurrentState().keyToItem(AgentRef).get(subagentItem.agentPath) do
          val agentDriver = agentRegister(agentRef.path).agentDriver
          agentDriver
            .changeAgentRef(agentRef)
            .recoverWith(t => IO(logger.error(
              s"$agentDriver.changeAgentRef => ${t.toStringWithCauses}", t.nullIfNoStackTrace)))
            .logWhenItTakesLonger(s"$agentDriver.changeAgentRef")
            .unsafeRunAndForget() // TODO
            //.awaitInfinite until v2.7

      case ItemDetached(itemKey, agentPath: AgentPath) =>
        for agentEntry <- agentRegister.get(agentPath) do
          agentEntry.detachingItems -= itemKey

      case ItemDeleted(agentPath: AgentPath) =>
        for entry <- agentRegister.get(agentPath) do
          entry.isDeleted = true
          val agentDriver = entry.agentDriver
          agentDriver.terminate(reset = true)
            .recoverWith(t => IO(logger.error(
              s"$agentDriver.terminate(reset) => ${t.toStringWithCauses}", t.nullIfNoStackTrace)))
            .logWhenItTakesLonger(s"$agentDriver.terminate(reset)")
            .unsafeRunAndForget() // TODO
          // Actor terminates asynchronously, so do not add an AgentRef immediately after deletion!

      case _ =>

  private def proceedWithItem(itemKey: InventoryItemKey): Unit =
    // TODO Handle AgentRef here: agentEntry .actor ! AgentDriver.Input.StartFetchingEvents ...
    for agentToAttachedState <- _controllerState.itemToAgentToAttachedState.get(itemKey) do
      for (agentPath, attachedState) <- agentToAttachedState do
        // TODO Does nothing if Agent is added later! (should be impossible, anyway)
        for agentEntry <- agentRegister.get(agentPath) do
          val agentDriver = agentEntry.agentDriver
          if !agentEntry.isResetting then
            attachedState match
              case Attachable =>
                itemKey match
                  case itemKey: SignableItemKey =>
                    for signedItem <- _controllerState.keyToSignedItem.get(itemKey) do
                      agentDriver
                        .send(AgentDriver.Queueable.AttachSignedItem(signedItem))
                        .recoverWith(t => IO(logger.error(
                          s"$agentDriver.send(AttachSignedItem) => ${t.toStringWithCauses}", t.nullIfNoStackTrace)))
                        .logAndIgnoreError(s"$agentDriver.send(AttachSignedItem)")
                        .awaitInfinite // TODO

                  case itemKey: UnsignedItemKey =>
                    for item <- _controllerState.keyToItem.get(itemKey) do
                      val unsignedItem = item.asInstanceOf[UnsignedItem]
                      agentDriver
                        .send(AgentDriver.Queueable.AttachUnsignedItem(unsignedItem))
                        .recoverWith(t => IO(logger.error(
                          s"$agentDriver.send(AttachUnsignedItem) => ${t.toStringWithCauses}", t.nullIfNoStackTrace)))
                        .logAndIgnoreError(s"$agentDriver.send(AttachUnsignedItem)")
                        .awaitInfinite // TODO

              case Detachable =>
                if /*!agentEntry.isDeleted && */!agentEntry.detachingItems.contains(itemKey) then
                  agentEntry.detachingItems += itemKey
                  agentDriver
                    .send(AgentDriver.Queueable.DetachItem(itemKey))
                    .recoverWith(t => IO(logger.error(
                      s"$agentDriver.send(DetachItem) => ${t.toStringWithCauses}", t.nullIfNoStackTrace)))
                    .logWhenItTakesLonger(s"$agentDriver.send(DetachItem)")
                    .awaitInfinite // TODO

              case _ =>

    // ResetSubagent
    itemKey match
      case subagentId: SubagentId =>
        for subagentItemState <- _controllerState.keyTo(SubagentItemState).get(subagentId) do
          subagentItemState.isResettingForcibly match
            case Some(force) =>
              for agentDriver <- agentRegister.get(subagentItemState.item.agentPath).map(_.agentDriver) do
                agentDriver
                  .send(AgentDriver.Queueable.ResetSubagent(subagentId, force))
                  .recoverWith(t => IO(logger.error(
                    s"$agentDriver.send(ResetSubagent) => ${t.toStringWithCauses}", t.nullIfNoStackTrace)))
                  .logWhenItTakesLonger(s"$agentDriver.send(ResetSubagent)")
                  .awaitInfinite // TODO
            case _ =>
      case _ =>
  end proceedWithItem

  private def handleOrderEvent(keyedEvent: KeyedEvent[OrderEvent]): Set[OrderId] =
    val KeyedEvent(orderId, event) = keyedEvent

    updateOrderEntry(orderId, event)

    event match
      case _: OrderAdded =>
        Set.empty

      case _ =>
        _controllerState.idToOrder.get(orderId) match
          case None =>
            logger.error(s"Unknown OrderId in event $keyedEvent")
            Set.empty

          case Some(order) =>
            val orderEventHandler = new OrderEventHandler(_controllerState.repo.idTo(Workflow))
            val checkedFollowUps = orderEventHandler.handleEvent(order, keyedEvent.event)
            val dependentOrderIds = mutable.Set.empty[OrderId]
            for followUps <- checkedFollowUps.onProblem(p => logger.error(p)) do  // TODO OrderBroken on error?
              followUps foreach:
                case FollowUp.AddChild(childOrder) =>
                  dependentOrderIds += childOrder.id

                case FollowUp.Delete(deleteOrderId) =>
                  for entry <- orderRegister.remove(deleteOrderId) do
                    entry.timer.cancel()

                case _: FollowUp.LeaveJob =>

            event match
              case OrderNoticePostedV2_3(notice) =>
                for boardPath <- _controllerState.workflowPositionToBoardPath(order.workflowPosition) do
                  val noticeId = PlanId.Global / boardPath / notice.noticeKey
                  notices.deleteSchedule(noticeId)
                  notices.schedule(noticeId, notice.endOfLife)

              case OrderNoticePosted(noticeId, endOfLife) =>
                notices.deleteSchedule(noticeId)
                notices.maybeSchedule(noticeId, endOfLife)

              case _ =>

            (dependentOrderIds.view ++
              (_controllerState.idToOrder.contains(orderId) ? order.id)
            ).toSet

  private def updateOrderEntry(orderId: OrderId, event: OrderEvent): Unit =
    val orderEntry = orderRegister.getOrElseUpdate(orderId, new OrderEntry(scheduler.now()))
    orderEntry.lastUpdatedAt = scheduler.now()
    event match
      case _: OrderAttachable | _: OrderDetachable =>
        orderEntry.triedToAttached = false

      case OrderGoes =>
        orderEntry.agentOrderMark match
          case Some(_: OrderMark.Go) => orderEntry.agentOrderMark = None
          case _ =>

      case OrderDetached =>
        orderEntry.isDetaching = false
        orderEntry.agentOrderMark = None

      case _ =>

  private def proceedWithOrders(orderIds: Iterable[OrderId]): Unit =
    if !shuttingDown && switchover.isEmpty then
      orderIds foreach proceedWithOrder

  private def proceedWithOrder(orderId: OrderId): Unit =
    for order <- _controllerState.idToOrder.get(orderId) do
      if order.isDetached then
        for until <- order.maybeDelayedUntil do
          alarmClock.lock:
            if until <= alarmClock.now() then
              orderQueue.enqueue(orderId :: Nil)
            else
              for entry <- orderRegister.get(orderId) do
                // TODO Cancel timer when unused
                entry.timer := alarmClock.scheduleAt(until, s"OrderIsDue($orderId)"):
                  self ! Internal.OrderIsDue(orderId)

      for mark <- order.mark do
        if order.isAttached
          && !orderRegister.get(orderId).flatMap(_.agentOrderMark).contains(mark) then
          // On Recovery, MarkOrder is sent again, because orderEntry.agentOrderMark is lost
          for (_, agentEntry) <- checkedWorkflowAndAgentEntry(order) do
            // CommandQueue filters multiple equal MarkOrder
            // because we may send multiple ones due to asynchronous execution

            // Special handling for OrderMark.Go: set agentOrderMark here, because when it's send
            // after AgentCommand.MarkOrder has been executed, then this may occur *after* the
            // OrderGoes event, too late, in the wrong order.
            mark match
              case _: OrderMark.Go =>
                orderRegister.get(orderId).foreach: orderEntry =>
                  orderEntry.agentOrderMark = Some(mark)
              case _ =>

            val agentDriver = agentEntry.agentDriver
            agentDriver
              .send(AgentDriver.Queueable.MarkOrder(order.id, mark))
              .recoverWith(t => IO(logger.error(
                s"$agentDriver.send(MarkOrder(${order.id})) => ${t.toStringWithCauses}", t.nullIfNoStackTrace)))
              .logWhenItTakesLonger(s"$agentDriver.send(MarkOrder(${order.id}))")
              .awaitInfinite // TODO

      order.attachedState match
        case Some(_: Order.Attaching) =>
          for order <- order.ifState[Order.IsFreshOrReady] do
            tryAttachOrderToAgent(order)

        case Some(_: Order.Detaching) =>
          detachOrderFromAgent(order.id)

        case _ =>

  private def delayOrderDeletion[E <: Event](keyedEvents: Seq[KeyedEvent[E]]): Seq[KeyedEvent[E]] =
    if deleteOrderDelay.isZeroOrBelow then
      keyedEvents
    else
      keyedEvents.filter:
        case KeyedEvent(orderId: OrderId, OrderDeleted) =>
          orderRegister.get(orderId).fold(false) { orderEntry =>
            val delay = orderEntry.lastUpdatedAt + deleteOrderDelay - scheduler.now()
            !delay.isPositive || {
              orderRegister(orderId).timer := scheduler.scheduleOnce(delay):
                self ! Internal.OrderIsDue(orderId)
              false
            }
            // When recovering, proceedWithOrderOnController may emit the same event multiple times,
            // for example OrderJoined for each parent and child order.
            // These events are collected and with actor message Internal.AfterProceedEventsAdded reduced to one.
          }
        case _ => true

  private def tryAttachOrderToAgent(order: Order[Order.IsFreshOrReady]): Unit =
    for (signedWorkflow, agentEntry) <- checkedWorkflowAndAgentEntry(order) do
      if order.isAttaching && !agentEntry.isResetting then
        val orderEntry = orderRegister(order.id)
        if !orderEntry.triedToAttached then
          val workflow = signedWorkflow.value
          import agentEntry.{agentDriver, agentPath}

          // Maybe attach Workflow
          val attachSignedItems: Seq[IO[Unit]] =
            workflow.referencedAttachableToAgentSignablePaths
              .flatMap(_controllerState.pathToSignedSimpleItem.get)
              .appended(signedWorkflow)
              .filter(signedItem => isDetachedOrAttachable(signedItem.value, agentPath))
              .map(signedItem =>
                agentDriver.send(AgentDriver.Queueable.AttachSignedItem(signedItem)))

          // Attach more required Items
          val attachUnsignedItems: Seq[IO[Unit]] =
            unsignedItemsToBeAttached(workflow, agentPath)
              .toVector
              .map(item =>
                agentDriver.send(AgentDriver.Queueable.AttachUnsignedItem(item)))

          // TODO AttachOrder mit parent orders!
          // Agent markiert die als bloß gebraucht für Kindaufträge
          // Mit Referenzzähler: der letzte Kindauftrag löscht seine Elternaufträge
          val attachOrder: IO[Unit] =
            agentDriver.send(AgentDriver.Queueable.AttachOrder(order, agentPath))

          orderEntry.triedToAttached = true

          // Now, IOs are calculcated from mutable state and and be started as a sequence:
          (attachSignedItems ++ attachUnsignedItems :+ attachOrder)
            .sequence
            .map(_.combineAll)
            .recoverWith(t => IO(logger.error(
              s"tryAttachOrderToAgent(${order.id}) => ${t.toStringWithCauses}", t.nullIfNoStackTrace)))
            .logWhenItTakesLonger(s"tryAttachOrderToAgent(${order.id})")
            .awaitInfinite // TODO

  private def unsignedItemsToBeAttached(workflow: Workflow, agentPath: AgentPath)
  : Iterable[UnsignedItem] =
    // TODO Optimize: Remember the set of already assigned ItemPaths and
    //  Items (which may change) for (workflow.id, agentPath)
    // (and clear with ResetAgent)
    val result = Map.newBuilder[UnsignedItemKey, UnsignedItem]

    result ++= workflow
      .reduceForAgent(agentPath)
      .referencedAttachableUnsignedPaths
      .view
      .flatMap(_controllerState.pathToUnsignedSimpleItem.get)
      .map(o => o.key -> o)

    // Workflow does not return those SubagentBundles which are referenced via
    // a variable expression.
    // So we attach all SubagentBundles which contain a SubagentId of the Agent
    result ++= _controllerState.keyToItem(SubagentBundle)
      .filter(_
        ._2.subagentIds
        .flatMap(_controllerState.keyToItem(SubagentItem).get)
        .exists(_.agentPath == agentPath))

    if _controllerState.workflowToOrders.workflowIdToOrders contains workflow.id then
      result ++= _controllerState.keyToItem(WorkflowPathControl)
        .get(WorkflowPathControlPath(workflow.path))
        .map(o => o.key -> o)

    result ++= _controllerState.keyTo(WorkflowControl).get(WorkflowControlId(workflow.id))
      .map(o => o.key -> o)

    result.result().values.view
      .filter(isDetachedOrAttachable(_, agentPath))

  private def checkedWorkflowAndAgentEntry(order: Order[Order.State])
  : Option[(Signed[Workflow], AgentEntry)] =
    order.attachedState match
      case Some(Order.AttachedState.HasAgentPath(agentPath)) =>
        ( for
            signedWorkflow <- _controllerState.repo.idToSigned(Workflow)(order.workflowId)
            agentEntry <- agentRegister.checked(agentPath)
          yield (signedWorkflow, agentEntry)
        ).onProblem(p => logger.error(p.withPrefix("checkedWorkflowAndAgentEntry:")))

      case _ => None

  private def isDetachedOrAttachable(item: InventoryItem, agentPath: AgentPath) =
    val attachedState = _controllerState.itemToAttachedState(item.key, item.itemRevision, agentPath)
    attachedState == Detached || attachedState == Attachable

  private def detachOrderFromAgent(orderId: OrderId): Unit =
    for orderEntry <- orderRegister.get(orderId) do
      if !orderEntry.isDetaching then
        _controllerState.idToOrder.checked(orderId)
          .flatMap(_.detaching)
          .onProblem(p => logger.error(s"detachOrderFromAgent '$orderId': not Detaching: $p"))
          .foreach { agentPath =>
            agentRegister.get(agentPath).map(_.agentDriver) match
              case None => logger.error(s"detachOrderFromAgent '$orderId': Unknown $agentPath")
              case Some(agentDriver) =>
                orderEntry.isDetaching = true
                agentDriver
                  .send(AgentDriver.Queueable.DetachOrder(orderId))
                  .recoverWith(t => IO(logger.error(
                    s"detachOrderFromAgent($orderId) => ${t.toStringWithCauses}", t.nullIfNoStackTrace)))
                  .logWhenItTakesLonger(s"detachOrderFromAgent($orderId)")
                  .awaitInfinite // TODO
          }

  private def instruction(workflowPosition: WorkflowPosition): Instruction =
    _controllerState.repo.idTo(Workflow)(workflowPosition.workflowId).orThrow
      .instruction(workflowPosition.position)

  private def clusterSwitchOver(restart: Boolean)
  : Future[Checked[ControllerCommand.Response.Accepted.type]] =
    if switchover.isDefined then
      Future.successful(Left(Problem("Already switching over")))
    else
      IO {
        new Switchover(restart = restart)
      } .bracketCase { so =>
          switchover = Some(so)
          so.start()
            .materialize.flatTap {
              case Success(Right(_)) => IO.unit  // this.switchover is left for postStop
              case _ => IO:
                switchover = None  // Asynchronous!
            }.dematerialize
        } ((so, exitCase) =>
          IO {
            logger.debug(s"SwitchOver => $exitCase")
            so.close()
          })
        .map(_.map((_: Completed) => ControllerCommand.Response.Accepted))
        .unsafeToFuture()

  private def orderEventSource = new OrderEventSource(_controllerState)

  private def runningAgentDriverCount =
    agentRegister.values.count(o => !o.actorTerminated)

  override def toString = "ControllerOrderKeeper"

private[controller] object ControllerOrderKeeper:
  private val logger = Logger[this.type]

  object Input:
    case object Start

  sealed trait Command
  object Command:
    final case class Execute(command: ControllerCommand, meta: CommandMeta, correlId: CorrelId)
    extends Command
    final case class VerifiedUpdateItemsCmd(verifiedUpdateRepo: VerifiedUpdateItems) extends Command
    final case class GetClusterWatchService(agentPath: AgentPath) extends Command

  private object Internal:
    case object ContinueWithNextOrderEvents extends DeadLetterSuppression
    final case class OrderIsDue(orderId: OrderId) extends DeadLetterSuppression
    final case class NoticeIsDue(noticeId: NoticeId) extends DeadLetterSuppression
    final case class Activated(recovered: Try[Unit])
    final case class ClusterModuleTerminatedUnexpectedly(tried: Try[Checked[Completed]])
      extends DeadLetterSuppression
    final case class Ready(outcome: Checked[Completed])
    case object StillShuttingDown extends DeadLetterSuppression
    final case class ShutDown(shutdown: ControllerCommand.ShutDown)
    final case class OrdersMarked(orderToMark: Map[OrderId, OrderMark])

    final case class EventsFromAgent(
      agentPath: AgentPath,
      agentRunId: AgentRunId,
      stamped: Seq[Stamped[AnyKeyedEvent]],
      promise: Promise[Option[EventId]])

    final case class AgentDriverStopped(agentPath: AgentPath)

  private implicit final class RichIdToOrder(private val idToOrder: Map[OrderId, Order[Order.State]])
  extends AnyVal:
    def checked(orderId: OrderId) = idToOrder.get(orderId).toChecked(UnknownOrderProblem(orderId))

  private case class AgentEntry(
    agentRef: AgentRef,
    allocatedAgentDriver: Allocated[IO, AgentDriver]):
    var actorTerminated = false
    var isResetting = false
    var isDeleted = false
    val detachingItems = mutable.Set.empty[InventoryItemKey]

    def agentPath = agentRef.path

    val agentDriver: AgentDriver =
      allocatedAgentDriver.allocatedThing

    def release: IO[Unit] =
      allocatedAgentDriver.release

  private class OrderEntry(now: SyncDeadline):
    var triedToAttached = false
    var isDetaching = false
    var lastUpdatedAt: SyncDeadline = now
    var agentOrderMark = none[OrderMark]
    val timer = SerialSyncCancelable()

  object ControllerReadyTestIncident
