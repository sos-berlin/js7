package js7.controller

import cats.effect.IO
import cats.effect.unsafe.{IORuntime, Scheduler}
import cats.instances.either.*
import cats.instances.vector.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.softwaremill.tagging.@@
import java.time.ZoneId
import js7.agent.data.commands.AgentCommand
import js7.agent.data.event.AgentEvent
import js7.base.catsutils.CatsEffectExtensions.{catchIntoChecked, now, right}
import js7.base.catsutils.CatsExtensions.{tryIt, untry}
import js7.base.catsutils.SyncDeadline
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.crypt.Signed
import js7.base.eventbus.EventPublisher
import js7.base.generic.Completed
import js7.base.log.Logger.ops.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixlike.MonixLikeExtensions.scheduleAtFixedRates
import js7.base.monixlike.{SerialSyncCancelable, SyncCancelable}
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.time.{AlarmClock, Timestamp, Timezone}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.Collections.implicits.InsertableMutableMap
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.base.utils.Tests.isStrict
import js7.base.utils.{Allocated, ProgramTermination, SetOnce}
import js7.cluster.WorkingClusterNode
import js7.common.pekkoutils.SupervisorStrategies
import js7.controller.ControllerOrderKeeper.*
import js7.controller.agent.{AgentDriver, AgentDriverConfiguration}
import js7.controller.command.ControllerCommandToEventCalc
import js7.controller.configuration.ControllerConfiguration
import js7.controller.problems.{ControllerIsNotReadyProblem, ControllerIsShuttingDownProblem, ControllerIsSwitchingOverProblem}
import js7.core.command.CommandMeta
import js7.core.problems.ReverseReleaseEventsProblem
import js7.data.Problems.{ClusterModuleShuttingDownProblem, UnknownOrderProblem}
import js7.data.agent.AgentRefStateEvent.{AgentEventsObserved, AgentMirroredEvent, AgentReady, AgentReset, AgentShutDown, AgentStarted}
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRunId}
import js7.data.board.NoticeEvent.{NoticeDeleted, NoticePosted}
import js7.data.board.{BoardPath, NoticeId}
import js7.data.calendar.{Calendar, CalendarExecutor}
import js7.data.cluster.ClusterEvent
import js7.data.command.IsEventEmittingCommand
import js7.data.controller.ControllerEvent.ControllerShutDown
import js7.data.controller.{ControllerCommand, ControllerEvent, ControllerState, ControllerStateExecutor, VerifiedUpdateItems, VerifiedUpdateItemsExecutor}
import js7.data.delegate.DelegateCouplingState
import js7.data.delegate.DelegateCouplingState.{Reset, Resetting}
import js7.data.event.JournalEvent.JournalEventsReleased
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventCalc, EventColl, EventId, KeyedEvent, Stamped, TimeCtx}
import js7.data.item.BasicItemEvent.{ItemAttached, ItemAttachedToMe, ItemDeleted, ItemDetached, ItemDetachingFromMe, SignedItemAttachedToMe}
import js7.data.item.ItemAttachedState.{Attachable, Detachable, Detached}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.item.{InventoryItem, InventoryItemEvent, InventoryItemKey, ItemAddedOrChanged, SignableItemKey, UnsignedItem, UnsignedItemKey}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancellationMarked, OrderCancellationMarkedOnAgent, OrderDetachable, OrderDetached, OrderGoes, OrderNoticePosted, OrderNoticePostedV2_3, OrderSuspensionMarked, OrderSuspensionMarkedOnAgent}
import js7.data.order.{Order, OrderEvent, OrderId, OrderMark}
import js7.data.orderwatch.OrderWatchEvent.ExternalOrderVanished
import js7.data.orderwatch.{OrderWatchEvent, OrderWatchPath, OrderWatchState}
import js7.data.plan.PlanEvent.{PlanDeleted, PlanFinished, PlanStatusEvent}
import js7.data.plan.PlanSchemaEvent.PlanSchemaChanged
import js7.data.plan.{PlanId, PlanSchemaId, PlanSchemaState, PlanStatus}
import js7.data.problems.UserIsNotEnabledToReleaseEventsProblem
import js7.data.state.OrderEventHandler
import js7.data.state.OrderEventHandler.FollowUp
import js7.data.subagent.SubagentItemStateEvent.{SubagentEventsObserved, SubagentResetStartedByController}
import js7.data.subagent.{SubagentBundle, SubagentId, SubagentItem, SubagentItemState, SubagentItemStateEvent}
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Instruction, Workflow, WorkflowControl, WorkflowControlId, WorkflowPathControl, WorkflowPathControlPath}
import js7.journal.{CommitOptions, JournalActor, JournalingActor, Persisted}
import org.apache.pekko.actor.{ActorRef, DeadLetterSuppression, Stash, Status, SupervisorStrategy}
import org.apache.pekko.pattern.pipe
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
  protected val journalActor: ActorRef @@ JournalActor.type,
  clusterNode: WorkingClusterNode[ControllerState],
  clock: AlarmClock,
  controllerConfiguration: ControllerConfiguration,
  testEventPublisher: EventPublisher[Any])
  (implicit protected val ioRuntime: IORuntime)
extends Stash, JournalingActor[ControllerState, Event]:

  import controllerConfiguration.config

  private given scheduler: Scheduler = ioRuntime.scheduler
  private given ExecutionContext = ioRuntime.compute

  override val supervisorStrategy: SupervisorStrategy = SupervisorStrategies.escalate
  protected def journalConf = controllerConfiguration.journalConf
  private val journal = clusterNode.journal

  private val controllerCommandToEventCalc = ControllerCommandToEventCalc(config)
  private val agentDriverConfiguration = AgentDriverConfiguration
    .fromConfig(config, controllerConfiguration.journalConf).orThrow

  private val agentRegister = mutable.Map[AgentPath, AgentEntry]()
  private val orderRegister = mutable.HashMap.empty[OrderId, OrderEntry]
  private val testAddOrderDelay = config
    .optionAs[FiniteDuration]("js7.TEST-ONLY.add-order-delay").fold(IO.unit)(IO.sleep)
  private var journalTerminated = false

  private object notices:
    private val noticeToSchedule = mutable.Map.empty[NoticeId, SyncCancelable]

    def maybeSchedule(noticeId: NoticeId, endOfLife: Option[Timestamp]): Unit =
      endOfLife.foreach:
        schedule(noticeId, _)

    def schedule(noticeId: NoticeId, endOfLife: Timestamp): Unit =
      noticeToSchedule.update(
        noticeId,
        clock.scheduleAt(endOfLife, s"NoticeIsDue($noticeId)"):
          self ! Internal.NoticeIsDue(noticeId))

    def deleteSchedule(noticeId: NoticeId): Unit =
      noticeToSchedule.remove(noticeId).foreach(_.cancel())

  private object plans:
    private val planToSchedule = mutable.Map.empty[PlanId, SyncCancelable]

    def schedule(planId: PlanId, ts: Timestamp): Unit =
      planToSchedule.update(
        planId,
        clock.scheduleAt(ts, s"PlanIsDue($planId)"):
          self ! Internal.PlanIsDue(planId))

    def deleteSchedule(planId: PlanId): Unit =
      planToSchedule.remove(planId).foreach(_.cancel())


  private object shutdown:
    var delayUntil = scheduler.now()
    val since = SetOnce[SyncDeadline]
    private val shutDown = SetOnce[ControllerCommand.ShutDown]
    private val stillShuttingDownCancelable = SerialSyncCancelable()
    private var terminatingAgentDrivers = false
    private var controllerShutdown = false

    def shuttingDown = since.isDefined

    def restart = shutDown.toOption.fold(false)(_.restart)

    def start(shutDown: ControllerCommand.ShutDown): Unit =
      if !shuttingDown then
        since := scheduler.now()
        this.shutDown := shutDown
        if shutDown.suppressSnapshot then
          journal.suppressSnapshotWhenStopping()
        stillShuttingDownCancelable := scheduler
          .scheduleAtFixedRates(controllerConfiguration.journalConf.ackWarnDurations/*?*/):
          self ! Internal.StillShuttingDown
        //BESSER ???  if shutDown.isFailover then
        if shutDown.isFailOrSwitchover then
          (journal.kill *> IO(context.stop(self)))
            .unsafeRunAndForget()
        else
          continue()

    def close(): Unit =
      stillShuttingDownCancelable.cancel()

    def onStillShuttingDown(): Unit =
      logger.info(s"Still shutting down, waiting for $runningAgentDriverCount AgentDrivers")

    def continue(): Unit =
      for shutDown <- shutDown do
        logger.trace(s"shutdown.continue: $runningAgentDriverCount AgentDrivers")
        if !terminatingAgentDrivers then
          terminatingAgentDrivers = true
          agentRegister.values.map(_.agentDriver)
            .toVector
            .parTraverse(agentDriver =>
              agentDriver.stop()
                .recoverWith(t => IO(logger.error(
                  s"$agentDriver.terminate => ${t.toStringWithCauses}", t.nullIfNoStackTrace)))
                .logWhenItTakesLonger(s"$agentDriver.terminate"))
            .unsafeRunAndForget() // TODO
        if runningAgentDriverCount == 0 then
          if !controllerShutdown && !shutDown.isFailOrSwitchover then
            // The event forces the cluster to acknowledge this event and the snapshot taken
            controllerShutdown = true
            persistKeyedEventIO(EventCalc.pure(NoKey <-: ControllerShutDown))(_ => Completed)
              .unsafeToFuture()
              .onComplete:
                case Success(Right(Completed)) => self ! Internal.ContinueShutdown
                case other => logger.error(s"While shutting down: $other")
          else
            context.stop(self)

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

    def start(): IO[Checked[Unit]] =
      clusterNode.switchOver   // Will terminate `cluster`, letting ControllerOrderKeeper terminate
        //? .flatMapT(o => journalAllocated.release.as(Right(o)))

    def close(): Unit = stillSwitchingOverSchedule.cancel()

  journal.untilStopped.productR(IO(self ! Internal.JournalStopped)).unsafeRunAndForget()

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
      val controllerState = journal.unsafeAggregate()
      if controllerState.controllerMetaState.isDefined then
        recover(controllerState)

      become("activating")(activating)
      unstashAll()
      self ! Internal.Activated

    case msg => notYetReady(msg)

  private def recover(controllerState: ControllerState): Unit =
    if controllerState.controllerId != controllerConfiguration.controllerId then
      throw Problem(s"Recovered '${controllerState.controllerId}' " +
        s"differs from configured '${controllerConfiguration.controllerId}'"
      ).throwable
    //controllerMetaState = controllerState.controllerMetaState.copy(totalRunningTime = recovered.totalRunningTime)

    for
      planSchemaState <- controllerState.keyTo(PlanSchemaState).values
      plan <- planSchemaState.plans
    do
      plan.status match
        case PlanStatus.Finished(at) =>
          plans.schedule(plan.id, at + planSchemaState.finishedPlanRetentionPeriod)
        case _ =>

    controllerState.allNotices.foreach: notice =>
      notices.maybeSchedule(notice.id, notice.endOfLife)

    persistedEventId = controllerState.eventId

  private def activating: Receive =
    case Internal.Activated =>
      // `become` must be called early, before any persist!
      become("becomingReady")(becomingReady)

      persist(
        EventCalc[ControllerState, Event]: coll =>
          for
            coll <- coll.add:
              !coll.aggregate.controllerMetaState.isDefined thenSome:
                NoKey <-: ControllerEvent.ControllerInitialized(
                  controllerConfiguration.controllerId,
                  journal.initiallyStartedAt)
            timezone <- Timezone.checked(ZoneId.systemDefault.getId)
            coll <- coll.add:
              NoKey <-: ControllerEvent.ControllerReady(
                timezone,
                totalRunningTime = journal.totalRunningTime)
            coll <- coll.add:
              ControllerStateExecutor.nextOrderWatchOrderEvents(coll.aggregate, coll.context.now)
          yield
            coll
      ) { persisted =>
        // Now, controllerState.controllerMetaState.controllerId is required for
        // automaticallyAppointConfiguredBackupNode

        clusterNode.afterAggregateInitialisation
          .catchIntoChecked
          .map(Internal.Ready.apply)
          .unsafeToFuture()
          .pipeTo(self)

        for path <- persisted.aggregate.keyToItem(AgentRef).keys do
          proceedWithItem(path)
        for path <- persisted.aggregate.keyTo(WorkflowPathControl).keys do
          proceedWithItem(path)
        for itemKey <- persisted.aggregate.keyTo(WorkflowControl).keys do
          proceedWithItem(itemKey)
      }

      val controllerState = this.controllerState()

      controllerState.keyTo(OrderWatchState).keys foreach proceedWithItem

      // Proceed order before starting AgentDrivers, so AgentDrivers may match recovered OrderIds with Agent's OrderIds
      orderRegister ++= controllerState.idToOrder.keys.map(_ -> new OrderEntry(scheduler.now()))

      // Start fetching events from Agents after AttachOrder has been sent to AgentDrivers.
      // This is to handle race-condition: An Agent may have already completed an order.
      // So send AttachOrder before DetachOrder.
      // The Agent will ignore the duplicate AttachOrder if it arrives before DetachOrder.
      for agentRef <- controllerState.pathToUnsignedSimple(AgentRef).values do
        val agentRefState = controllerState.keyTo(AgentRefState)
          .getOrElse(agentRef.path, AgentRefState(agentRef))
        registerAgent(agentRef, eventId = agentRefState.eventId)

      // Any ordering when continuing orders???
      proceedWithOrders(controllerState.idToOrder.keys)
      orderQueue.enqueue(controllerState.idToOrder.keys)

      if persistedEventId > EventId.BeforeFirst then // Recovered?
        logger.info(s"${controllerState.idToOrder.size} Orders, " +
          s"${controllerState.repo.typedCount[Workflow]} Workflows and " +
          s"${controllerState.keyTo(AgentRefState).size} AgentRefs recovered")

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

    case Internal.Ready(Right(())) =>
      logger.info(s"${controllerState().controllerId} is ready\n" + "─" * 80)
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
      persist(
        ControllerStateExecutor.nextOrderEvents(orderIds),
        CommitOptions.Transaction
      )(handleEvents)

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
        for agentRefState <- journal.unsafeAggregate().keyTo(AgentRefState).get(agentPath) do
          val isAgentReset = agentRefState.couplingState match
            case _: DelegateCouplingState.Resetting => true
            case DelegateCouplingState.Reset.byCommand => true
            case _ => false

          committedPromise.completeWith:
            if isAgentReset /*Race condition ??? Use persist(EventCalc)! */ then
              for o <- stampedAgentEvents.map(_.value) do logger.warn(
                s"Ignored event after Agent reset: $o")
              Future.successful(None)
            else if !agentRefState.agentRunId.forall(_ == agentRunId) then
              logger.debug(s"Internal.EventsFromAgent: Unknown agentRunId=$agentRunId")
              Future.successful(None)
            else
              val eventCalc = EventCalc[ControllerState, Event]: coll =>
                // TODO On error, invalidate only affected Order or Agent
                stampedAgentEvents.view.foldEithers(coll):
                  case (coll, Stamped(_, ts, keyedEvent)) =>
                    keyedEvent match
                      case KeyedEvent(orderId: OrderId, _: OrderCancellationMarked) =>
                        coll.add:
                          orderId <-: OrderCancellationMarkedOnAgent ^ ts

                      case KeyedEvent(orderId: OrderId, _: OrderSuspensionMarked) =>
                        coll.add:
                          orderId <-: OrderSuspensionMarkedOnAgent ^ ts

                      case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
                        coll.add:
                          val evt = orderId <-: event.match
                            // TODO Das kann schon der Agent machen. Dann wird weniger übertragen.
                            case _: OrderEvent.OrderAttachedToAgent => OrderAttached(agentPath)
                            case _ => event
                          evt ^ ts

                      case KeyedEvent(_: NoKey, AgentEvent.AgentStarted) =>
                        coll.add:
                          agentEntry.agentPath <-: AgentStarted ^ ts

                      case KeyedEvent(_: NoKey, AgentEvent.AgentReady(timezone, _, platformInfo)) =>
                        coll.add:
                          agentEntry.agentPath <-: AgentReady(timezone, platformInfo) ^ ts

                      case KeyedEvent(_: NoKey, AgentEvent.AgentShutDown) =>
                        coll.add:
                          agentEntry.agentPath <-: AgentShutDown ^ ts

                      case KeyedEvent(_: NoKey, ItemAttachedToMe(item)) =>
                        coll.add:
                          // COMPATIBLE with v2.1
                          NoKey <-: ItemAttached(item.key, item.itemRevision, agentPath)

                      case KeyedEvent(_: NoKey, SignedItemAttachedToMe(signed)) =>
                        coll.add:
                          // TODO Das kann schon der Agent machen. Dann wird weniger übertragen.
                          val item = signed.value
                          NoKey <-: ItemAttached(item.key, item.itemRevision, agentPath)

                      case KeyedEvent(_: NoKey, _: ItemDetachingFromMe) =>
                        Right(coll)

                      case KeyedEvent(_: NoKey, _: ItemDetached) =>
                        coll.add(keyedEvent)

                      case KeyedEvent(path: OrderWatchPath, event: OrderWatchEvent) =>
                        coll.add(keyedEvent).flatMap: coll =>
                          event match
                            case event: ExternalOrderVanished =>
                              coll.add:
                                coll.aggregate.ow.toOrderExternalVanished(path, event)

                            case _ =>
                              Right(coll)

                      case KeyedEvent(_, _: ItemAddedOrChanged) =>
                        Right(coll)

                      case KeyedEvent(_: SubagentId, event: SubagentItemStateEvent) =>
                        coll.add:
                          event match
                            case _: SubagentEventsObserved => None // Not needed
                            case _ => Some(keyedEvent)

                      case ke @ KeyedEvent(_: NoKey, _: ClusterEvent) =>
                        coll.add:
                          agentPath <-: AgentMirroredEvent(ke)

                      case _ =>
                        logger.error:
                          s"Unknown event received from ${agentEntry.agentPath}: $keyedEvent"
                        Right(coll)

              val agentEventId = stampedAgentEvents.last.eventId
              persist(
                EventCalc: coll =>
                  for
                    coll <- coll.addEventCalc(eventCalc)
                    coll <-
                      coll.add:
                        // timestampedEvents may be empty if it contains only discarded (Agent-only) events.
                        // Agent's last observed EventId is not persisted then, and we do not write an AgentEventsObserved.
                        // For tests, this makes the journal predictable after OrderFinished (because no AgentEventsObserved may follow).
                        coll.hasEvents thenSome
                          agentPath <-: AgentEventsObserved(agentEventId)
                    coll <-
                      coll.addEventCalc:
                        addSubsequentEvents
                  yield
                    coll,
                CommitOptions(
                  transaction = true,
                  alreadyDelayed = agentDriverConfiguration.eventBufferDelay)
              ): persisted =>
                orderQueue.enqueue:
                  persisted.keyedEvents.view.collect:
                    case KeyedEvent(orderId: OrderId, _) => orderId // For OrderSourceEvents
                handleEvents(persisted)
                Some(agentEventId)

    case Internal.OrdersMarked(orderToMark) =>
      // TODO Maybe execute this code when the corresponding event arrives:
      // Like already OrderGoMarked: OrderSuspensionMarked, OrderResumptionMarked, ...
      // Then we must not handle a late AgentCommand.MarkOrder response.
      val unknown = orderToMark -- controllerState().idToOrder.keySet
      if unknown.nonEmpty then
        logger.error("Response to AgentCommand.MarkOrder from Agent for unknown orders: " +
          unknown.mkString(", "))
      orderToMark.foreach:
        case (orderId, _: OrderMark.Go) =>
          // Do not set agentOrderMark. This would defeat OrderGoes, which clears agentOrderMark.
          // OrderGoes event may arrive before MarkOrders response.
        case (orderId, mark) =>
          orderRegister(orderId).agentOrderMark = Some(mark)

    case Internal.OrderIsDue(orderId) =>
      proceedWithOrders(orderId :: Nil)
      orderQueue.enqueue(orderId :: Nil)

    case Internal.NoticeIsDue(noticeId) =>
      notices.deleteSchedule(noticeId)
      for
        plan <- controllerState().toPlan.get(noticeId.planId);
        plannedBoard <- plan.toPlannedBoard.get(noticeId.boardPath)
        notice <- plannedBoard.maybeNotice(noticeId.noticeKey)
        keyedEvent <- plannedBoard.deleteNoticeEvent(noticeId.noticeKey).toOption
      do
        if notice.endOfLife.exists(clock.now() < _) then
          notices.maybeSchedule(noticeId, notice.endOfLife)
        else
          logger.debug(s"Notice lifetime expired: $noticeId")
          persist(EventCalc.pure(keyedEvent))(handleEvents)

    case Internal.PlanIsDue(planId) =>
      plans.deleteSchedule(planId)
      persist(
        EventCalc.multiple[ControllerState, Event]: controllerState =>
          for
            planSchemaState <- controllerState.keyTo(PlanSchemaState).get(planId.planSchemaId).toList
            plan <- planSchemaState.plan(planId.planKey).toOption.toList
            keyedEvents <-
              plan.status match
                case PlanStatus.Finished(at) =>
                  (at + planSchemaState.finishedPlanRetentionPeriod <= clock.now()).thenList:
                    planId <-: PlanDeleted  // FIXME Event hier?
                case _ => Nil
          yield
            keyedEvents
      )(handleEvents)

    case Internal.ShutDown(shutDown) =>
      shutdown.delayUntil = scheduler.now() + config.getDuration("js7.web.server.delay-shutdown")
        .toFiniteDuration
      shutdown.start(shutDown)

    case Internal.ContinueShutdown =>
      shutdown.continue()

    case Internal.StillShuttingDown =>
      shutdown.onStillShuttingDown()

    case Internal.AgentDriverStopped(agentPath) if agentRegister contains agentPath =>
      var agentEntry = agentRegister(agentPath)
      agentEntry.actorTerminated = true
      agentEntry.release.unsafeRunAndForget()/*???*/ // Release in case there are surrounding Resources
      if switchover.isDefined && /*journalTerminated &&*/ runningAgentDriverCount == 0 then
        val delay = shutdown.delayUntil.timeLeft
        if delay.isPositive then
          logger.debug(s"Sleep ${delay.pretty} after ShutDown command")
          sleep(delay)
        context.stop(self)
      else if shuttingDown then
        shutdown.continue()
      else
        agentRegister -= agentPath
        for agentRefState <- journal.unsafeAggregate().keyTo(AgentRefState).checked(agentPath) do
          agentRefState.couplingState match
            case Resetting(_) | Reset(_) =>
              agentEntry = registerAgent(agentRefState.agentRef, eventId = EventId.BeforeFirst)
            //??? reattachToAgent(agentPath)

            case _ =>
              logger.debug(s"AgentDriver for $agentPath terminated")

  private def executeVerifiedUpdateItems(verifiedUpdateItems: VerifiedUpdateItems): Unit =
    val t = scheduler.now()
    (for
      keyedEvents <- VerifiedUpdateItemsExecutor.execute(verifiedUpdateItems, controllerState(), {
        case calendar: Calendar =>
          CalendarExecutor.checked(calendar, Timezone.utc/*irrelevant*/).rightAs(())
      })
      coll <- EventColl(controllerState(), TimeCtx(clock.now())).add(keyedEvents)
      _ <- checkAgentDriversAreTerminated(
        keyedEvents.view
          .collect { case KeyedEvent(_, UnsignedSimpleItemAdded(a: AgentRef)) => a.path })
    yield coll)
    match
      case Left(problem) =>
        sender() ! Left(problem)

      case Right(coll) =>
        val sender = this.sender()
        persistTransactionAndSubsequentEvents(coll)(handleEvents)
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
    case Internal.JournalStopped =>
      journalTerminated = true
      if !shuttingDown && switchover.isEmpty then logger.error("JournalActor terminated")
      if switchover.isDefined && runningAgentDriverCount > 0 then
        agentRegister.values.map(_.agentDriver) foreach { agentDriver =>
          agentDriver
            .stop(noJournal = true)
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
        case Success(checked: Checked[Unit]) =>
          val msg: Any = checked.fold(identity, identity)
          logger.error(s"Cluster module terminated unexpectedly: $msg")
        case Failure(t) =>
          logger.error(s"Cluster module terminated unexpectedly: ${t.toStringWithCauses}", t)
      if shuttingDown then
        shutdown.continue()
      else
        context.stop(self)

  private def executeControllerCommand(command: ControllerCommand, commandMeta: CommandMeta)
  : Future[Checked[ControllerCommand.Response]] =
    command match
      case cmd: (
        ControllerCommand.AnswerOrderPrompt |
          ControllerCommand.CancelOrders |
          ControllerCommand.ChangePlan |
          ControllerCommand.ChangePlanSchema |
          ControllerCommand.DeleteNotice |
          ControllerCommand.DeleteOrdersWhenTerminated |
          ControllerCommand.ChangeGlobalToPlannableBoard |
          ControllerCommand.ChangePlannableToGlobalBoard |
          ControllerCommand.EmitTestEvent |
          ControllerCommand.GoOrder |
          ControllerCommand.PostNotice |
          ControllerCommand.ResumeOrder |
          ControllerCommand.ResumeOrders |
          ControllerCommand.SuspendOrders |
          ControllerCommand.ChangeOrder |
          ControllerCommand.TransferOrders) =>
        executeCommandAndPersist(cmd)

      case cmd: ControllerCommand.AddOrder =>
        executeCommandAndPersistAndHandle(cmd): persisted =>
          if persisted.isEmpty then
            logger.debug(s"Discarding duplicate added Order: ${cmd.order}")
            Right(ControllerCommand.AddOrder.Response(ignoredBecauseDuplicate = true))
          else
            handleEvents(persisted)
            Right(ControllerCommand.AddOrder.Response(ignoredBecauseDuplicate = false))
        .flatMap: o =>
          testAddOrderDelay.unsafeToFuture().map(_ => o) // test only

      case cmd: ControllerCommand.AddOrders =>
        executeCommandAndPersistAndHandle(cmd): persisted =>
          handleEvents(persisted)
          // Emit subsequent events later for earlier addOrders response (and smaller event chunk)
          orderQueue.enqueue(cmd.orders.view.map(_.id))
          Right(ControllerCommand.AddOrders.Response(persisted.aggregate.eventId))

      case cmd: ControllerCommand.ControlWorkflowPath =>
        controlWorkflowPath(cmd)

      case cmd: ControllerCommand.ControlWorkflow =>
        controlWorkflow(cmd)

      case ControllerCommand.ReleaseEvents(untilEventId) =>
        val userId = commandMeta.user.id
        if !controllerConfiguration.journalConf.releaseEventsUserIds.contains(userId) then
          Future(Left(UserIsNotEnabledToReleaseEventsProblem))
        else
          val current = controllerState().journalState.userIdToReleasedEventId.getOrElse(userId, EventId.BeforeFirst)
          if untilEventId < current then
            Future(Left(ReverseReleaseEventsProblem(requestedUntilEventId = untilEventId, currentUntilEventId = current)))
          else
            persistKeyedEvent(JournalEventsReleased(userId, untilEventId)) { (_, updatedState) =>
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
        journal.takeSnapshot
          .as(Right(ControllerCommand.Response.Accepted))
          .unsafeToFuture()

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

      case ControllerCommand.ResetAgent(agentPath, force) =>
        agentRegister.checked(agentPath) match
          case Left(problem) => Future.successful(Left(problem))
          case Right(agentEntry) =>
            journal.unsafeAggregate().keyTo(AgentRefState).checked(agentEntry.agentPath) match
              case Left(problem) => Future.successful(Left(problem))
              case Right(agentRefState) =>
                // TODO journal.lock(agentPath), to avoid race with AgentCoupled, too
                // As a workaround, AgentRefState.applyEvent ignores AgentCoupled if Resetting
                agentRefState.couplingState match
                  case Resetting(frc) if !force || frc != force =>
                    Future.successful(Left(Problem.pure("AgentRef is already in Resetting state")))
                  case reset: Reset if !force =>
                    Future.successful(Left(Problem.pure(s"AgentRef is already in $reset state")))
                  case _ =>
                    ControllerStateExecutor.startResetAgent(agentPath, force = force)
                      .calculate(journal.unsafeAggregate(), TimeCtx(Timestamp.now))
                    match
                      case Left(problem) => Future.successful(Left(problem))
                      case Right(coll) =>
                        persistTransactionAndSubsequentEvents(coll) { persisted =>
                          // ResetAgent command may return with error despite it has reset the orders
                          agentEntry.isResetting = true
                          handleEvents(persisted)
                          agentEntry.agentDriver.reset(force = force)
                            .catchIntoChecked
                            .flatTap: _ =>
                              // Stop AgentDriver even if reset failed
                              agentEntry.agentDriver.stop()
                            .flatMap:
                              case Left(problem) =>
                                IO.right(logger.warn:
                                  s"Delaying $agentPath AgentCommand.Reset due to: $problem")
                              case Right(()) =>
                                journal.persist(_
                                  .keyTo(AgentRefState).checked(agentPath)
                                  .map(_.couplingState)
                                  .map:
                                    case Resetting(_) => (agentPath <-: AgentReset) :: Nil
                                    case _ => Nil)
                            .rightAs(ControllerCommand.Response.Accepted)
                            .unsafeToFuture()
                        }.flatten

      case ControllerCommand.ResetSubagent(subagentId, force) =>
        val controllerState = journal.unsafeAggregate()
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
              persist(EventCalc.pure(events)) { _ =>
                proceedWithItem(subagentId)
              }.map(_ => Right(ControllerCommand.Response.Accepted))

      case ControllerCommand.ClusterSwitchOver(Some(agentPath)) =>
        agentRegister.checked(agentPath)
          .map(_.agentDriver)
          .match
            case Left(problem) => Future.successful(Left(problem))
            case Right(agentDriver) =>
              agentDriver
                .executeCommandDirectly(AgentCommand.ClusterSwitchOver)
                .logWhenItTakesLonger(s"$agentDriver.send(ClusterSwitchOver)")
                .catchIntoChecked
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
        // Handled by ControllerCommandToEventCalc
        Future.failed(new NotImplementedError)

  private def controlWorkflowPath(cmd: ControllerCommand.ControlWorkflowPath)
  : Future[Checked[ControllerCommand.Response]] =
    val path = WorkflowPathControlPath(cmd.workflowPath)
    executeCommandAndPersistAndHandle(cmd): persisted =>
      // Continue even if WorkflowPathControl is not changed.
      // This allows the caller to force the redistribution of the WorkflowPathControl.
      handleEvents(persisted)
      proceedWithItem(path)
      val workflowPathControl = persisted.aggregate.keyTo(WorkflowPathControl)(path)
      if !workflowPathControl.item.suspended then
        orderQueue.enqueue(
          persisted.aggregate.orders.filter(_.workflowPath == workflowPathControl.workflowPath).map(_.id))
      Right(ControllerCommand.Response.Accepted)

  private def controlWorkflow(cmd: ControllerCommand.ControlWorkflow)
  : Future[Checked[ControllerCommand.Response]] =
    val workflowControlId = WorkflowControlId(cmd.workflowId)
    executeCommandAndPersistAndHandle(cmd): persisted =>
      handleEvents(persisted)
      proceedWithItem(workflowControlId)
      Right(ControllerCommand.Response.Accepted)

  private def executeCommandAndPersist(cmd: ControllerCommand & IsEventEmittingCommand)
  : Future[Checked[ControllerCommand.Response.Accepted]] =
    executeCommandAndPersistAndHandle(cmd): persisted =>
      handleEvents(persisted)
      Right(ControllerCommand.Response.Accepted)

  private def executeCommandAndPersistAndHandle[R <: ControllerCommand.Response](
    cmd: ControllerCommand & IsEventEmittingCommand)
    (handle: Persisted[ControllerState, Event] => Checked[R])
  : Future[Checked[R]] =
    executeCommandAndPersistAndHandle2[R](
      controllerCommandToEventCalc.commandToEventCalc(cmd)
    )(handle)

  private def executeCommandAndPersistAndHandle2[R <: ControllerCommand.Response](
    eventCalc: EventCalc[ControllerState, Event])
    (handle: Persisted[ControllerState, Event] => Checked[R])
  : Future[Checked[R]] =
    eventCalc.calculate(controllerState(), TimeCtx(clock.now())) match
      case Left(problem) => Future.successful(Left(problem))
      case Right(coll) =>
        persistTransactionAndSubsequentEvents(coll): persisted =>
          handle(persisted)

  private def registerAgent(agent: AgentRef, eventId: EventId): AgentEntry =
    val allocated = AgentDriver
      .service(agent, eventId = eventId,
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
      .*>(IO:
        self ! Internal.AgentDriverStopped(agent.path))
      .unsafeRunAndForget() // TODO

    val entry = AgentEntry(agent, allocated)
    agentRegister.insert(agent.path, entry)
    entry

  private def persistTransactionAndSubsequentEvents[A](coll: EventColl[ControllerState, Event])
    (callback: Persisted[ControllerState, Event] => A)
  : Future[A] =
    persist(
      EventCalc.pure:
        addSubsequentEvents.calculate(coll).orThrow.timestampedKeyedEvents,
      CommitOptions.Transaction
    )(callback)

  private def addSubsequentEvents: EventCalc[ControllerState, Event] =
    EventCalc: coll =>
      ControllerStateExecutor.addSubsequentEvents(coll)

  private def handleEvents(persisted: Persisted[ControllerState, Event]): Unit =
    val itemKeys = mutable.Set.empty[InventoryItemKey]
    val orderIds = mutable.Set.empty[OrderId]
    var controllerState = persisted.originalAggregate
    for stamped <- persisted.stampedKeyedEvents do
      val keyedEvent = stamped.value
      keyedEvent match
        case KeyedEvent(orderId: OrderId, _: OrderEvent) =>
          orderIds += orderId
          orderIds ++= handleOrderEvent(controllerState, keyedEvent.asInstanceOf[KeyedEvent[OrderEvent]])
          controllerState = controllerState.applyKeyedEvent(keyedEvent).orThrow

        case KeyedEvent(_: NoKey, event: InventoryItemEvent) =>
          controllerState = controllerState.applyKeyedEvent(keyedEvent).orThrow
          itemKeys += event.key
          handleItemEvent(event)

        case KeyedEvent(boardPath: BoardPath, noticePosted: NoticePosted) =>
          val noticeId = boardPath / noticePosted.plannedNoticeKey
          notices.deleteSchedule(noticeId)
          notices.maybeSchedule(noticeId, noticePosted.endOfLife)
          controllerState = controllerState.applyKeyedEvent(keyedEvent).orThrow

        case KeyedEvent(boardPath: BoardPath, NoticeDeleted(plannedNoticeKey)) =>
          notices.deleteSchedule(boardPath / plannedNoticeKey)
          controllerState = controllerState.applyKeyedEvent(keyedEvent).orThrow

        case KeyedEvent(planId: PlanId, event: PlanStatusEvent) =>
          controllerState = controllerState.applyKeyedEvent(keyedEvent).orThrow
          event match
            case PlanFinished(at) =>
              for
                planSchemaState <- controllerState.keyTo(PlanSchemaState).get(planId.planSchemaId)
              do
                plans.schedule(planId, at + planSchemaState.finishedPlanRetentionPeriod)
            case _ =>
              plans.deleteSchedule(planId)

        case KeyedEvent(planSchemaId: PlanSchemaId, PlanSchemaChanged(Some(finishedPlanRetentionPeriod), _)) =>
          controllerState = controllerState.applyKeyedEvent(keyedEvent).orThrow
          for
            planSchemaState <- controllerState.keyTo(PlanSchemaState).get(planSchemaId).toSeq
            plan <- planSchemaState.plans
          do
            plan.status match
              case PlanStatus.Finished(at) => plans.schedule(plan.id, at + finishedPlanRetentionPeriod)
              case _ => plans.deleteSchedule(plan.id)

        case _ =>
          controllerState = controllerState.applyKeyedEvent(keyedEvent).orThrow
    if isStrict then assertThat(controllerState.withEventId(persisted.aggregate.eventId) == persisted.aggregate)
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
        for agentRef <- journal.unsafeAggregate().keyToItem(AgentRef).get(subagentItem.agentPath) do
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
          agentDriver.resetAgentAndStop
            .recoverWith(t => IO(logger.error(
              s"$agentDriver.terminate(reset) => ${t.toStringWithCauses}", t.nullIfNoStackTrace)))
            .logWhenItTakesLonger(s"$agentDriver.terminate(reset)")
            .unsafeRunAndForget() // TODO
          // Actor terminates asynchronously, so do not add an AgentRef immediately after deletion!

      case _ =>

  private def proceedWithItem(itemKey: InventoryItemKey): Unit =
    // TODO Handle AgentRef here: agentEntry .actor ! AgentDriver.Input.StartFetchingEvents ...
    for agentToAttachedState <- controllerState().itemToAgentToAttachedState.get(itemKey) do
      for (agentPath, attachedState) <- agentToAttachedState do
        // TODO Does nothing if Agent is added later! (should be impossible, anyway)
        for agentEntry <- agentRegister.get(agentPath) do
          val agentDriver = agentEntry.agentDriver
          if !agentEntry.isResetting then
            attachedState match
              case Attachable =>
                itemKey match
                  case itemKey: SignableItemKey =>
                    for signedItem <- controllerState().keyToSignedItem.get(itemKey) do
                      agentDriver
                        .send(AgentDriver.Queueable.AttachSignedItem(signedItem))
                        .recoverWith(t => IO(logger.error(
                          s"$agentDriver.send(AttachSignedItem) => ${t.toStringWithCauses}", t.nullIfNoStackTrace)))
                        .logAndIgnoreError(s"$agentDriver.send(AttachSignedItem)")
                        .awaitInfinite // TODO

                  case itemKey: UnsignedItemKey =>
                    for item <- controllerState().keyToItem.get(itemKey) do
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
        for subagentItemState <- controllerState().keyTo(SubagentItemState).get(subagentId) do
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

  private def handleOrderEvent(controllerState: ControllerState, keyedEvent: KeyedEvent[OrderEvent]): Set[OrderId] =
    val KeyedEvent(orderId, event) = keyedEvent

    updateOrderEntry(orderId, event)

    event match
      case _: OrderAdded =>
        Set.empty

      case _ =>
        controllerState.idToOrder.get(orderId) match
          case None =>
            logger.error(s"Unknown OrderId in event $keyedEvent")
            Set.empty

          case Some(order) =>
            val orderEventHandler = new OrderEventHandler(controllerState.repo.idTo(Workflow))
            val checkedFollowUps = orderEventHandler.handleEvent(order, keyedEvent.event)
            val dependentOrderIds = mutable.Set.empty[OrderId]
            for followUps <- checkedFollowUps.problemToNone(p => logger.error(p)) do  // TODO OrderBroken on error?
              followUps foreach:
                case FollowUp.AddChild(childOrder) =>
                  dependentOrderIds += childOrder.id

                case FollowUp.Delete(deleteOrderId) =>
                  for entry <- orderRegister.remove(deleteOrderId) do
                    entry.timer.cancel()

                case _: FollowUp.LeaveJob =>

            event match
              case OrderNoticePostedV2_3(notice) =>
                for boardPath <- controllerState.workflowPositionToBoardPath(order.workflowPosition) do
                  val noticeId = PlanId.Global / boardPath / notice.noticeKey
                  notices.deleteSchedule(noticeId)
                  notices.schedule(noticeId, notice.endOfLife)

              case OrderNoticePosted(noticeId, endOfLife) =>
                notices.deleteSchedule(noticeId)
                notices.maybeSchedule(noticeId, endOfLife)

              case _ =>

            (dependentOrderIds.view ++
              (controllerState.idToOrder.contains(orderId) ? order.id)
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
    for order <- controllerState().idToOrder.get(orderId) do
      if order.isDetached then
        for until <- order.maybeDelayedUntil do
          clock.lock:
            if until <= clock.now() then
              orderQueue.enqueue(orderId :: Nil)
            else
              for entry <- orderRegister.get(orderId) do
                // TODO Cancel timer when unused
                entry.timer := clock.scheduleAt(until, s"OrderIsDue($orderId)"):
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
              .flatMap(controllerState().pathToSignedSimpleItem.get)
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

          // Now, IOs are calculated from mutable state and and be started as a sequence:
          (attachSignedItems ++ attachUnsignedItems :+ attachOrder)
            .sequence
            .map(_.combineAll)
            .recoverWith(t => IO(logger.error(
              s"tryAttachOrderToAgent(${order.id}) => ${t.toStringWithCauses}", t.nullIfNoStackTrace)))
            .logWhenItTakesLonger(s"tryAttachOrderToAgent(${order.id})")
            .awaitInfinite // TODO

  private def unsignedItemsToBeAttached(workflow: Workflow, agentPath: AgentPath)
  : Iterable[UnsignedItem] =
    // OPTIMISE: Remember the set of already assigned ItemPaths and
    //  Items (which may change) for (workflow.id, agentPath)
    // (and clear with ResetAgent)
    val result = Map.newBuilder[UnsignedItemKey, UnsignedItem]

    result ++= workflow
      .reduceForAgent(agentPath)
      .referencedAttachableUnsignedPaths
      .view
      .flatMap(controllerState().pathToUnsignedSimpleItem.get)
      .map(o => o.key -> o)

    // Workflow does not return those SubagentBundles which are referenced via
    // a variable expression.
    // So we attach all SubagentBundles which contain a SubagentId of the Agent
    result ++= controllerState().keyToItem(SubagentBundle)
      .filter(_
        ._2.subagentIds
        .flatMap(controllerState().keyToItem(SubagentItem).get)
        .exists(_.agentPath == agentPath))

    if controllerState().workflowToOrders.workflowIdToOrders contains workflow.id then
      result ++= controllerState().keyToItem(WorkflowPathControl)
        .get(WorkflowPathControlPath(workflow.path))
        .map(o => o.key -> o)

    result ++= controllerState().keyTo(WorkflowControl).get(WorkflowControlId(workflow.id))
      .map(o => o.key -> o)

    result.result().values.view
      .filter(isDetachedOrAttachable(_, agentPath))

  private def checkedWorkflowAndAgentEntry(order: Order[Order.State])
  : Option[(Signed[Workflow], AgentEntry)] =
    order.attachedState match
      case Some(Order.AttachedState.HasAgentPath(agentPath)) =>
        ( for
            signedWorkflow <- controllerState().repo.idToSigned(Workflow)(order.workflowId)
            agentEntry <- agentRegister.checked(agentPath)
          yield (signedWorkflow, agentEntry)
        ).problemToNone(p => logger.error(p.withPrefix("checkedWorkflowAndAgentEntry:")))

      case _ => None

  private def isDetachedOrAttachable(item: InventoryItem, agentPath: AgentPath) =
    val attachedState = controllerState().itemToAttachedState(item.key, item.itemRevision, agentPath)
    attachedState == Detached || attachedState == Attachable

  private def detachOrderFromAgent(orderId: OrderId): Unit =
    for orderEntry <- orderRegister.get(orderId) do
      if !orderEntry.isDetaching then
        controllerState().idToOrder.checked(orderId)
          .flatMap(_.detaching)
          .problemToNone(p => logger.error(s"detachOrderFromAgent '$orderId': not Detaching: $p"))
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
    controllerState().repo.idTo(Workflow)(workflowPosition.workflowId).orThrow
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
            .tryIt.flatTap {
              case Success(Right(_)) =>
                context.stop(self)
                IO.unit  // this.switchover is left for postStop
              case _ => IO:
                switchover = None  // Asynchronous!
            }.untry
        } ((so, exitCase) =>
          IO {
            logger.debug(s"SwitchOver => $exitCase")
            so.close()
          })
        .rightAs(ControllerCommand.Response.Accepted)
        .unsafeToFuture()

  private def runningAgentDriverCount =
    agentRegister.values.count(o => !o.actorTerminated)

  private def controllerState() =
    journal.unsafeAggregate()

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
    final case class PlanIsDue(planId: PlanId) extends DeadLetterSuppression
    case object Activated
    final case class ClusterModuleTerminatedUnexpectedly(tried: Try[Checked[Unit]])
      extends DeadLetterSuppression
    final case class Ready(outcome: Checked[Unit])
    case object ContinueShutdown extends DeadLetterSuppression
    case object StillShuttingDown extends DeadLetterSuppression
    final case class ShutDown(shutdown: ControllerCommand.ShutDown)
    final case class OrdersMarked(orderToMark: Map[OrderId, OrderMark])
    case object JournalStopped

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
