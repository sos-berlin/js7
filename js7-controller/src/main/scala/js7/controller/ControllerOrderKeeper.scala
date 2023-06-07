package js7.controller

import akka.actor.{DeadLetterSuppression, Stash, Status, Terminated}
import akka.pattern.{ask, pipe}
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
import js7.base.configutils.Configs.ConvertibleConfig
import js7.base.crypt.Signed
import js7.base.eventbus.EventPublisher
import js7.base.generic.Completed
import js7.base.log.Logger.ops.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.monixutils.MonixDeadline
import js7.base.monixutils.MonixDeadline.now
import js7.base.monixutils.MonixDeadline.syntax.*
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.time.{AlarmClock, Timezone}
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.Collections.implicits.{InsertableMutableMap, RichIterable}
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.StackTraces.StackTraceThrowable
import js7.base.utils.{Allocated, ProgramTermination, SetOnce}
import js7.cluster.WorkingClusterNode
import js7.common.akkautils.SupervisorStrategies
import js7.common.system.startup.ServiceMain
import js7.controller.ControllerOrderKeeper.*
import js7.controller.agent.{AgentDriver, AgentDriverConfiguration}
import js7.controller.configuration.ControllerConfiguration
import js7.controller.problems.{ControllerIsNotReadyProblem, ControllerIsShuttingDownProblem, ControllerIsSwitchingOverProblem}
import js7.core.command.CommandMeta
import js7.core.problems.ReverseReleaseEventsProblem
import js7.data.Problems.{CannotDeleteChildOrderProblem, CannotDeleteWatchingOrderProblem, UnknownOrderProblem}
import js7.data.agent.AgentRefStateEvent.{AgentEventsObserved, AgentMirroredEvent, AgentReady, AgentReset, AgentShutDown}
import js7.data.agent.{AgentPath, AgentRef, AgentRefState, AgentRunId}
import js7.data.board.BoardEvent.{NoticeDeleted, NoticePosted}
import js7.data.board.{BoardPath, BoardState, Notice, NoticeId}
import js7.data.calendar.{Calendar, CalendarExecutor}
import js7.data.cluster.ClusterEvent
import js7.data.controller.ControllerCommand.{ControlWorkflow, ControlWorkflowPath, TransferOrders}
import js7.data.controller.ControllerEvent.{ControllerShutDown, ControllerTestEvent}
import js7.data.controller.ControllerStateExecutor.convertImplicitly
import js7.data.controller.{ControllerCommand, ControllerEvent, ControllerState, VerifiedUpdateItems, VerifiedUpdateItemsExecutor}
import js7.data.delegate.DelegateCouplingState
import js7.data.delegate.DelegateCouplingState.{Reset, Resetting}
import js7.data.event.JournalEvent.JournalEventsReleased
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, Stamped}
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.{InstructionExecutorService, PostNoticesExecutor}
import js7.data.item.BasicItemEvent.{ItemAttached, ItemAttachedToMe, ItemDeleted, ItemDetached, ItemDetachingFromMe, SignedItemAttachedToMe}
import js7.data.item.ItemAttachedState.{Attachable, Detachable, Detached}
import js7.data.item.UnsignedItemEvent.{UnsignedItemAdded, UnsignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemChanged}
import js7.data.item.{InventoryItem, InventoryItemEvent, InventoryItemKey, ItemAddedOrChanged, ItemRevision, SignableItemKey, UnsignedItem, UnsignedItemKey}
import js7.data.order.OrderEvent.{OrderActorEvent, OrderAdded, OrderAttachable, OrderAttached, OrderCancellationMarked, OrderCancellationMarkedOnAgent, OrderCoreEvent, OrderDeleted, OrderDeletionMarked, OrderDetachable, OrderDetached, OrderNoticePosted, OrderNoticePostedV2_3, OrderSuspensionMarked, OrderSuspensionMarkedOnAgent}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, OrderMark}
import js7.data.orderwatch.{OrderWatchEvent, OrderWatchPath, OrderWatchState}
import js7.data.problems.UserIsNotEnabledToReleaseEventsProblem
import js7.data.state.OrderEventHandler
import js7.data.state.OrderEventHandler.FollowUp
import js7.data.subagent.SubagentItemStateEvent.{SubagentEventsObserved, SubagentResetStartedByController}
import js7.data.subagent.{SubagentId, SubagentItem, SubagentItemState, SubagentItemStateEvent, SubagentSelection}
import js7.data.value.expression.scopes.NowScope
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Instruction, Workflow, WorkflowControl, WorkflowControlId, WorkflowPathControl, WorkflowPathControlPath}
import js7.journal.state.FileJournal
import js7.journal.{CommitOptions, JournalActor, MainJournalingActor}
import monix.eval.Task
import monix.execution.cancelables.SerialCancelable
import monix.execution.{Cancelable, Scheduler}
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
final class ControllerOrderKeeper(
  stopped: Promise[ProgramTermination],
  journalAllocated: Allocated[Task, FileJournal[ControllerState]],
  clusterNode: WorkingClusterNode[ControllerState],
  alarmClock: AlarmClock,
  controllerConfiguration: ControllerConfiguration,
  testEventPublisher: EventPublisher[Any])
  (implicit protected val scheduler: Scheduler)
extends Stash
with MainJournalingActor[ControllerState, Event]
{
  import context.watch
  import controllerConfiguration.config
  import js7.controller.ControllerOrderKeeper.RichIdToOrder

  override val supervisorStrategy = SupervisorStrategies.escalate
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
    .optionAs[FiniteDuration]("js7.TEST-ONLY.add-order-delay").fold(Task.unit)(Task.sleep)
  private var journalTerminated = false

  private object notices {
    private val noticeToSchedule = mutable.Map.empty[(BoardPath, NoticeId), Cancelable]

    def schedule(notice: Notice): Unit =
      noticeToSchedule += notice.boardPath -> notice.id ->
        alarmClock.scheduleAt(notice.endOfLife) {
          self ! Internal.NoticeIsDue(notice.boardPath, notice.id)
        }

    def deleteSchedule(boardPath: BoardPath, noticeId: NoticeId): Unit =
      noticeToSchedule.remove(boardPath -> noticeId).foreach(_.cancel())
  }

  private object shutdown {
    var delayUntil = now
    val since = SetOnce[MonixDeadline]
    private val shutDown = SetOnce[ControllerCommand.ShutDown]
    private val stillShuttingDownCancelable = SerialCancelable()
    private var terminatingAgentDrivers = false
    private var takingSnapshot = false
    private var snapshotTaken = false
    private var terminatingJournal = false

    def shuttingDown = since.isDefined

    def restart = shutDown.toOption.fold(false)(_.restart)

    def start(shutDown: ControllerCommand.ShutDown): Unit =
      if (!shuttingDown) {
        since := now
        this.shutDown := shutDown
        stillShuttingDownCancelable := scheduler
          .scheduleAtFixedRates(controllerConfiguration.journalConf.ackWarnDurations/*?*/) {
          self ! Internal.StillShuttingDown
        }
        continue()
      }

    def close() =
      stillShuttingDownCancelable.cancel()

    def onStillShuttingDown() =
      logger.info(s"Still shutting down, waiting for $runningAgentDriverCount AgentDrivers" +
        (!snapshotTaken ?? " and the snapshot"))

    def onSnapshotTaken(): Unit =
      if (shuttingDown) {
        snapshotTaken = true
        continue()
      }

    def continue() =
      for (shutDown <- shutDown) {
        logger.trace(s"shutdown.continue: $runningAgentDriverCount AgentDrivers${
          !snapshotTaken ?? ", snapshot required"}")
        if (!terminatingAgentDrivers) {
          terminatingAgentDrivers = true
          agentRegister.values.map(_.agentDriver)
            .toVector
            .parTraverse(agentDriver =>
              agentDriver.terminate()
                .onErrorHandle(t => logger.error(
                  s"$agentDriver.terminate => ${t.toStringWithCauses}", t.nullIfNoStackTrace))
                .logWhenItTakesLonger(s"$agentDriver.terminate"))
            .runAsyncAndForget // TODO
        }
        if (runningAgentDriverCount == 0) {
          if (!takingSnapshot) {
            takingSnapshot = true
            if (shutDown.suppressSnapshot) {
              snapshotTaken = true
            } else {
              journalActor ! JournalActor.Input.TakeSnapshot
            }
          }
          if (snapshotTaken && !terminatingJournal) {
            // The event forces the cluster to acknowledge this event and the snapshot taken
            terminatingJournal = true
            persistKeyedEventTask(NoKey <-: ControllerShutDown)((_, _) => Completed)
              .tapEval(_ => journalAllocated.release)
              .runToFuture
              .onComplete {
                case Success(Right(Completed)) =>
                case other => logger.error(s"While shutting down: $other")
              }
          }
        }
      }
  }
  import shutdown.shuttingDown

  /** Next orders to be processed. */
  private object orderQueue {
    private val queue = new VectorBuilder[OrderId]
    private val known = mutable.Set.empty[OrderId]
    private var notified = false

    def enqueue(orderIds: Iterable[OrderId]): Unit =
      if (!shuttingDown && switchover.isEmpty && orderIds.nonEmpty) {
        for (orderId <- orderIds.iterator) {
          if (known.add(orderId)) {
            queue += orderId
          }
        }
        if (!notified) {
          self ! Internal.ContinueWithNextOrderEvents
          notified = true
        }
      }

    def readAll(): Seq[OrderId] = {
      notified = false
      val orderIds = queue.result()
      queue.clear()
      known.clear()
      orderIds
    }

    override def toString = queue.result().map(_.string).mkString(", ")  // For debugging
  }

  @volatile
  private var switchover: Option[Switchover] = None

  private final class Switchover(val restart: Boolean) {
    // 1) Emit SwitchedOver event
    // 2) Terminate JournalActor
    // 3) Stop ControllerOrderKeeper includinge AgentDriver's
    // Do not terminate AgentDrivers properly because we do not want any events.

    private val stillSwitchingOverSchedule = scheduler
      .scheduleAtFixedRates(controllerConfiguration.journalConf.ackWarnDurations) {
      logger.debug("Still switching over to the other cluster node")
    }

    def start(): Task[Checked[Completed]] =
      clusterNode.switchOver   // Will terminate `cluster`, letting ControllerOrderKeeper terminate
        .flatMapT(o => journalAllocated.release.as(Right(o)))

    def close() = stillSwitchingOverSchedule.cancel()
  }

  watch(journalActor)

  override def postStop() =
    try {
      shutdown.close()
      switchover foreach { _.close() }
    } finally {
      logger.debug(
        "Stopped" + shutdown.since.toOption.fold("")(o => s" (terminated in ${o.elapsed.pretty})"))
      stopped.success(
        ProgramTermination(restart = switchover.exists(_.restart) | shutdown.restart))
      super.postStop()
    }

  def receive = {
    case Input.Start =>
      val controllerState = journal.unsafeCurrentState()
      if (controllerState.controllerMetaState.isDefined) {
        recover(controllerState)
      }

      become("activating")(activating)
      unstashAll()
      clusterNode.beforeJournalingStarts
        .map(_.orThrow)
        .materialize
        .map(Internal.Activated.apply)
        .runToFuture
        .pipeTo(self)

    case msg => notYetReady(msg)
  }

  private def recover(controllerState: ControllerState): Unit = {
    if (controllerState.controllerId != controllerConfiguration.controllerId)
      throw Problem(s"Recovered '${controllerState.controllerId}' " +
        s"differs from configured '${controllerConfiguration.controllerId}'"
      ).throwable
    this._controllerState = controllerState
    //controllerMetaState = controllerState.controllerMetaState.copy(totalRunningTime = recovered.totalRunningTime)

    for (
      boardState <- controllerState.keyTo(BoardState).values;
      notice <- boardState.notices)
    {
      notices.schedule(notice)
    }

    persistedEventId = controllerState.eventId
}

  private def activating: Receive = {
    case Internal.Activated(Failure(t)) =>
      logger.error(s"Activation of this cluster node failed because: ${t.toStringWithCauses}")
      if (t.getStackTrace.nonEmpty) logger.debug(t.toStringWithCauses, t)
      throw t.appendCurrentStackTrace

    case Internal.Activated(Success(())) =>
      // `become` must be called early, before any persist!
      become("becomingReady")(becomingReady)

      locally {
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
            .runToFuture
            .pipeTo(self)

          for (path <- _controllerState.keyTo(WorkflowPathControl).keys) {
            proceedWithItem(path)
          }
          for (itemKey <- _controllerState.keyTo(WorkflowControl).keys) {
            proceedWithItem(itemKey)
          }
        }
      }

      _controllerState.keyTo(OrderWatchState).keys foreach proceedWithItem

      // Proceed order before starting AgentDrivers, so AgentDrivers may match recovered OrderIds with Agent's OrderIds
      orderRegister ++= _controllerState.idToOrder.keys.map(_ -> new OrderEntry(scheduler.now))

      // Start fetching events from Agents after AttachOrder has been sent to AgentDrivers.
      // This is to handle race-condition: An Agent may have already completed an order.
      // So send AttachOrder before DetachOrder.
      // The Agent will ignore the duplicate AttachOrder if it arrives before DetachOrder.
      for (agentRef <- _controllerState.pathToUnsignedSimple(AgentRef).values) {
        val agentRefState = _controllerState.keyTo(AgentRefState)
          .getOrElse(agentRef.path, AgentRefState(agentRef))
        registerAgent(agentRef, eventId = agentRefState.eventId)
      }

      // Any ordering when continuing orders???
      proceedWithOrders(_controllerState.idToOrder.keys)
      orderQueue.enqueue(_controllerState.idToOrder.keys)

      if (persistedEventId > EventId.BeforeFirst) { // Recovered?
        logger.info(s"${_controllerState.idToOrder.size} Orders, " +
          s"${_controllerState.repo.typedCount[Workflow]} Workflows and " +
          s"${_controllerState.keyTo(AgentRefState).size} AgentRefs recovered")
      }

    case Command.Execute(_: ControllerCommand.ShutDown, _, _) =>
      stash()

    case Command.Execute(cmd, _, correlId) =>
      correlId.bind {
        logger.warn(s"$ControllerIsNotReadyProblem: $cmd")
      }
      sender() ! Left(ControllerIsNotReadyProblem)

    case cmd: Command =>
      logger.warn(s"$ControllerIsNotReadyProblem: $cmd")
      sender() ! Status.Failure(ControllerIsNotReadyProblem.throwable)

    case msg => notYetReady(msg)
  }

  private def notYetReady(message: Any): Unit =
    message match {
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
    }

  private def becomingReady: Receive = {
    case Internal.Ready(Left(problem)) =>
      logger.error(s"Appointment of configured cluster backup-node failed: $problem")
      throw problem.throwable.appendCurrentStackTrace

    case Internal.Ready(Right(Completed)) =>
      logger.info(ServiceMain.readyMessageWithLine(s"${_controllerState.controllerId} is ready"))
      testEventPublisher.publish(ControllerReadyTestIncident)
      clusterNode.onTerminatedUnexpectedly.runToFuture onComplete { tried =>
        self ! Internal.ClusterModuleTerminatedUnexpectedly(tried)
      }
      become("Ready")(ready orElse handleExceptionalMessage)
      unstashAll()

    case _ =>
      // stash Command too, after ControllerReady event and cluster node has been initialized (see above)
      stash()
  }

  private def ready: Receive = {
    case Internal.ContinueWithNextOrderEvents =>
      val orderIds = orderQueue.readAll()
      val keyedEvents = nextOrderEvents(orderIds)
      if (keyedEvents.nonEmpty) {
        persistTransaction(keyedEvents)(handleEvents)
      }

    case Command.Execute(command, meta, correlId) =>
      val sender = this.sender()
      if (shuttingDown)
        sender ! Status.Success(Left(ControllerIsShuttingDownProblem))
      else if (switchover.isDefined)
        sender ! Status.Success(Left(ControllerIsSwitchingOverProblem))
      else
        correlId.bind(
          executeControllerCommand(command, meta)
        ).onComplete {
          case Failure(t) => sender ! Status.Failure(t)
          case Success(response) => sender ! response
        }

    case Command.VerifiedUpdateItemsCmd(verifiedUpdateItems: VerifiedUpdateItems) =>
      executeVerifiedUpdateItems(verifiedUpdateItems)

    case Internal.EventsFromAgent(agentPath, agentRunId, stampedAgentEvents, committedPromise) =>
      for (agentEntry <- agentRegister.get(agentPath)) {
        for (agentRefState <- journal.unsafeCurrentState().keyTo(AgentRefState).get(agentPath)) {
          val isAgentReset = agentRefState.couplingState match {
            case _: DelegateCouplingState.Resetting => true
            case DelegateCouplingState.Reset.byCommand => true
            case _ => false
          }
          committedPromise.completeWith(if (isAgentReset/*Race condition ???*/) {
            for (o <- stampedAgentEvents.map(_.value)) logger.warn(
              s"Ignored event after Agent reset: $o")
            Future.successful(None)
          } else if (!agentRefState.agentRunId.forall(_ == agentRunId)) {
            logger.debug(s"Internal.EventsFromAgent: Unknown agentRunId=$agentRunId")
            Future.successful(None)
          } else {
            var timestampedEvents: Seq[Timestamped[Event]] =
              stampedAgentEvents.view.flatMap {
                case Stamped(_, timestampMillis, keyedEvent) =>
                  keyedEvent match {
                    case KeyedEvent(orderId: OrderId, _: OrderCancellationMarked) =>
                      Timestamped(orderId <-: OrderCancellationMarkedOnAgent, Some(timestampMillis)) :: Nil

                    case KeyedEvent(orderId: OrderId, _: OrderSuspensionMarked) =>
                      Timestamped(orderId <-: OrderSuspensionMarkedOnAgent, Some(timestampMillis)) :: Nil

                    case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
                      val ownEvent = event match {
                        case _: OrderEvent.OrderAttachedToAgent => OrderAttached(agentPath) // TODO Das kann schon der Agent machen. Dann wird weniger übertragen.
                        case _ => event
                      }
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
                      event match {
                        case _: SubagentEventsObserved => Nil  // Not needed
                        case _ => Timestamped(keyedEvent) :: Nil
                      }

                    case ke @ KeyedEvent(_: NoKey, _: ClusterEvent) =>
                      Timestamped(agentPath <-: AgentMirroredEvent(ke)) :: Nil

                    case _ =>
                      logger.error(s"Unknown event received from ${agentEntry.agentPath}: $keyedEvent")
                      Nil
                  }
              }.toVector

              if (timestampedEvents.isEmpty) {
                // timestampedEvents may be empty if it contains only discarded (Agent-only) events.
                // Agent's last observed EventId is not persisted then, and we do not write an AgentEventsObserved.
                // For tests, this makes the journal predictable after OrderFinished (because no AgentEventsObserved may follow).
                Future.successful(None)
              } else {
                val agentEventId = stampedAgentEvents.last.eventId
                timestampedEvents :+= Timestamped(agentPath <-: AgentEventsObserved(agentEventId))

                val subseqEvents = subsequentEvents(timestampedEvents.map(_.keyedEvent))
                orderQueue.enqueue(
                  subseqEvents.view.collect { case KeyedEvent(orderId: OrderId, _) => orderId })  // For OrderSourceEvents
                timestampedEvents ++= subseqEvents.map(Timestamped(_))

                journal.unsafeCurrentState().keyTo(AgentRefState).get(agentPath).map(_.couplingState) match {
                  case Some(DelegateCouplingState.Resetting(_) | DelegateCouplingState.Reset(_)) =>
                    // Ignore the events, because orders are already marked as detached (and Failed)
                    // TODO Avoid race-condition and guard with journal.lock!
                    // (switch from actors to Task required!)
                    Future.successful(None)
                  case _ =>
                    persistTransactionTimestamped(timestampedEvents,
                      CommitOptions(alreadyDelayed = agentDriverConfiguration.eventBufferDelay))
                    {
                      (stampedEvents, updatedState) =>
                        handleEvents(stampedEvents, updatedState)
                        Some(agentEventId)
                    }
                }
              }
          })
        }
      }

    case Internal.OrdersMarked(orderToMark) =>
      val unknown = orderToMark -- _controllerState.idToOrder.keySet
      if (unknown.nonEmpty) {
        logger.error("Response to AgentCommand.MarkOrder from Agent for unknown orders: " +
          unknown.mkString(", "))
      }
      for ((orderId, mark) <- orderToMark) {
        orderRegister(orderId).agentOrderMark = Some(mark)
      }

    case JournalActor.Output.SnapshotTaken =>
      shutdown.onSnapshotTaken()

    case Internal.OrderIsDue(orderId) =>
      proceedWithOrders(orderId :: Nil)
      orderQueue.enqueue(orderId :: Nil)

    case Internal.NoticeIsDue(boardPath, noticeId) =>
      notices.deleteSchedule(boardPath, noticeId)
      for (
        boardState <- _controllerState.keyTo(BoardState).checked(boardPath);
        notice <- boardState.notice(noticeId);
        keyedEvent <- boardState.deleteNoticeEvent(noticeId))
      {
        if (alarmClock.now() < notice.endOfLife) {
          notices.schedule(notice)
        } else {
          logger.debug(s"Notice lifetime expired: $boardPath $noticeId")
          persistMultiple(keyedEvent :: Nil)(handleEvents)
        }
      }

    case Internal.ShutDown(shutDown) =>
      shutdown.delayUntil = now + config.getDuration("js7.web.server.delay-shutdown")
        .toFiniteDuration
      shutdown.start(shutDown)

    case Internal.StillShuttingDown =>
      shutdown.onStillShuttingDown()

    case Internal.AgentDriverStopped(agentPath) if agentRegister contains agentPath =>
      var agentEntry = agentRegister(agentPath)
      agentEntry.actorTerminated = true
      agentEntry.release.runAsyncAndForget/*???*/ // Release in case there are surrounding Resources
      if (switchover.isDefined && journalTerminated && runningAgentDriverCount == 0) {
        val delay = shutdown.delayUntil.timeLeft
        if (delay.isPositive) {
          logger.debug(s"Sleep ${delay.pretty} after ShutDown command")
          sleep(delay)
        }
        context.stop(self)
      } else if (shuttingDown) {
        shutdown.continue()
      } else {
        agentRegister -= agentPath
        for (agentRefState <- journal.unsafeCurrentState().keyTo(AgentRefState).checked(agentPath)) {
          agentRefState.couplingState match {
            case Resetting(_) | Reset(_) =>
              agentEntry = registerAgent(agentRefState.agentRef, eventId = EventId.BeforeFirst)
            //??? reattachToAgent(agentPath)

            case _ =>
              logger.debug(s"AgentDriver for $agentPath terminated")
          }
        }
      }
  }

  private def executeVerifiedUpdateItems(verifiedUpdateItems: VerifiedUpdateItems): Unit = {
    val t = now
    (for {
      keyedEvents <- VerifiedUpdateItemsExecutor.execute(verifiedUpdateItems, _controllerState, {
        case calendar: Calendar => CalendarExecutor.checked(calendar).rightAs(())
      })
      _ <- checkAgentDriversAreTerminated(
        keyedEvents.view
          .collect { case KeyedEvent(_, UnsignedSimpleItemAdded(a: AgentRef)) => a.path })
    } yield keyedEvents)
    match {
      case Left(problem) =>
        sender() ! Left(problem)

      case Right(keyedEvents) =>
        val sender = this.sender()
        persistTransactionAndSubsequentEvents(keyedEvents)(handleEvents)
          .map(_ => Right(Completed))
          .map(_.map { o =>
            if (t.elapsed > 1.s) logger.debug("VerifiedUpdateItemsCmd - " +
              itemsPerSecondString(t.elapsed, verifiedUpdateItems.itemCount, "items"))
            o
          })
          .onComplete {
            case Failure(t) => sender ! Status.Failure(t)
            case Success(response) => sender ! (response: Checked[Completed])
          }
    }
  }

  private def checkAgentDriversAreTerminated(addedAgentPaths: Iterable[AgentPath])
  : Checked[Unit] = {
    val runningAgentDrivers = addedAgentPaths.filter(agentRegister.contains)
    if (runningAgentDrivers.nonEmpty)
      Left(Problem("AgentDrivers for the following Agents are still running — " +
        s"please retry after some seconds: ${runningAgentDrivers.map(_.string).mkString(", ")}"))
    else
      Checked.unit
  }

  // JournalActor's termination must be handled in any `become`-state and
  // must lead to ControllerOrderKeeper's termination
  override def journaling = handleExceptionalMessage orElse super.journaling

  private def handleExceptionalMessage: Receive = {
    case Terminated(actor) if actor == journalActor =>
      journalTerminated = true
      if (!shuttingDown && switchover.isEmpty) logger.error("JournalActor terminated")
      if (switchover.isDefined && runningAgentDriverCount > 0) {
        agentRegister.values.map(_.agentDriver) foreach { agentDriver =>
          agentDriver
            .terminate(noJournal = true)
            .onErrorHandle(t => logger.error(
              s"$agentDriver.terminate => ${t.toStringWithCauses}", t.nullIfNoStackTrace))
            .logWhenItTakesLonger(s"$agentDriver.terminate")
            .runAsyncAndForget // TODO
        }
      } else {
        context.stop(self)
      }

    case Internal.ClusterModuleTerminatedUnexpectedly(tried) =>
      // Stacktrace has been debug-logged by Cluster
      tried match {
        case Success(checked: Checked[Completed]) =>
          val msg: Any = checked.fold(identity, identity)
          logger.error(s"Cluster module terminated unexpectedly: $msg")
        case Failure(t) =>
          logger.error(s"Cluster module terminated unexpectedly: ${t.toStringWithCauses}", t)
      }
      context.stop(self)
  }

  private def executeControllerCommand(command: ControllerCommand, commandMeta: CommandMeta)
  : Future[Checked[ControllerCommand.Response]] =
    command match {
      case ControllerCommand.AddOrder(order) =>
        if (shuttingDown)
          Future.successful(Left(ControllerIsShuttingDownProblem))
        else if (switchover.isDefined)
          Future.successful(Left(ControllerIsSwitchingOverProblem))
        else
          addOrder(order)
            .map(_.map(added => ControllerCommand.AddOrder.Response(ignoredBecauseDuplicate = !added)))

      case ControllerCommand.AddOrders(orders) =>
        if (shuttingDown)
          Future.successful(Left(ControllerIsShuttingDownProblem))
        else if (switchover.isDefined)
          Future.successful(Left(ControllerIsSwitchingOverProblem))
        else
          addOrders(orders).map(_.map(eventId =>
            ControllerCommand.AddOrders.Response(eventId)))

      case ControllerCommand.CancelOrders(orderIds, mode) =>
        executeOrderMarkCommands(orderIds.toVector)(orderEventSource.cancel(_, mode))

      case ControllerCommand.SuspendOrders(orderIds, mode) =>
        executeOrderMarkCommands(orderIds.toVector)(orderEventSource.suspend(_, mode))

      case ControllerCommand.ResumeOrder(orderId, position, historicOps, asSucceeded) =>
        executeOrderMarkCommands(Vector(orderId))(
          orderEventSource.resume(_, position, historicOps, asSucceeded))

      case cmd: ControllerCommand.TransferOrders =>
        executeTransferOrders(cmd)

      case cmd: ControllerCommand.ControlWorkflowPath =>
        controlWorkflowPath(cmd)

      case cmd: ControllerCommand.ControlWorkflow =>
        controlWorkflow(cmd)

      case ControllerCommand.ResumeOrders(orderIds, asSucceeded) =>
        executeOrderMarkCommands(orderIds.toVector)(
          orderEventSource.resume(_, None, Nil, asSucceeded))

      case ControllerCommand.PostNotice(boardPath, noticeId, maybeEndOfLife) =>
        val scope = NowScope(alarmClock.now())
        val checked = for {
          boardState <- _controllerState.keyTo(BoardState).checked(boardPath)
          notice <- boardState.board.toNotice(noticeId, maybeEndOfLife)(scope)
          _ <- boardState.addNotice(notice) // Check
          expectingOrderEvents <- PostNoticesExecutor
            .postedNoticeToExpectingOrderEvents(boardState, notice, _controllerState)
        } yield (notice, expectingOrderEvents)
        checked match {
          case Left(problem) => Future.successful(Left(problem))
          case Right((notice, expectingOrderEvents)) =>
            val events = NoticePosted.toKeyedEvent(notice) +: expectingOrderEvents
            persistTransactionAndSubsequentEvents(events)(
              handleEvents
            ).map(_ => Right(ControllerCommand.Response.Accepted))
        }

      case ControllerCommand.DeleteNotice(boardPath, noticeId) =>
        (for {
          boardState <- _controllerState.keyTo(BoardState).checked(boardPath)
          keyedEvent <- boardState.deleteNoticeEvent(noticeId)
        } yield keyedEvent)
        match {
          case Left(problem) => Future.successful(Left(problem))
          case Right(keyedEvent) =>
            persistTransactionAndSubsequentEvents(keyedEvent :: Nil)(handleEvents)
             .map(_ => Right(ControllerCommand.Response.Accepted))
        }

      case ControllerCommand.DeleteOrdersWhenTerminated(orderIds) =>
        orderIds.toVector
          .traverse(_controllerState.idToOrder.checked)
          .traverse(orders =>
            orders.traverse(order =>
              if (order.parent.isDefined)
                Left(CannotDeleteChildOrderProblem(order.id): Problem)
              else if (order.externalOrderKey.isDefined)
                Left(CannotDeleteWatchingOrderProblem(order.id): Problem)
              else
                Right(order)))
          .flatten
          .map(_
            .filterNot(_.deleteWhenTerminated)
            .map(orderDeletedEvent))
          .traverse(keyedEvents =>
            persistTransactionAndSubsequentEvents(keyedEvents)(handleEvents)
              .map(_ => ControllerCommand.Response.Accepted))

      case ControllerCommand.ReleaseEvents(untilEventId) =>
        val userId = commandMeta.user.id
        if (!controllerConfiguration.journalConf.releaseEventsUserIds.contains(userId))
          Future(Left(UserIsNotEnabledToReleaseEventsProblem))
        else {
          val current = _controllerState.journalState.userIdToReleasedEventId.getOrElse(userId, EventId.BeforeFirst)
          if (untilEventId < current)
            Future(Left(ReverseReleaseEventsProblem(requestedUntilEventId = untilEventId, currentUntilEventId = current)))
          else
            persist(JournalEventsReleased(userId, untilEventId)) { (_, updatedState) =>
              _controllerState = updatedState
              Right(ControllerCommand.Response.Accepted)
            }
        }

      case ControllerCommand.NoOperation(maybeDuration) =>
        // NoOperation completes only after ControllerOrderKeeper has become ready
        // (can be used to await readiness)
        Task.pure(Right(ControllerCommand.Response.Accepted))
          .delayExecution(maybeDuration getOrElse 0.s)
          .runToFuture

      case _: ControllerCommand.EmergencyStop | _: ControllerCommand.Batch =>
        // For completeness. RunningController has handled the command already
        Future.successful(Left(Problem.pure("THIS SHOULD NOT HAPPEN")))  // Never called

      case ControllerCommand.TakeSnapshot =>
        import controllerConfiguration.implicitAkkaAskTimeout  // We need several seconds or even minutes
        intelliJuseImport(implicitAkkaAskTimeout)
        (journalActor ? JournalActor.Input.TakeSnapshot)
          .mapTo[JournalActor.Output.SnapshotTaken.type]
          .map(_ => Right(ControllerCommand.Response.Accepted))

      case ControllerCommand.ClusterSwitchOver(None) =>
        clusterSwitchOver(restart = true)

      case shutDown: ControllerCommand.ShutDown =>
        shutDown.clusterAction match {
          case Some(ControllerCommand.ShutDown.ClusterAction.Switchover) =>
            clusterSwitchOver(restart = shutDown.restart)

          case Some(ControllerCommand.ShutDown.ClusterAction.Failover) =>
            // TODO ClusterState.Coupled !
            shutdown.start(shutDown)
            Future.successful(Right(ControllerCommand.Response.Accepted))

          case None =>
            clusterNode.shutDownThisNode
              .flatTap {
                case Right(Completed) => Task { self ! Internal.ShutDown(shutDown) }
                case _ => Task.unit
              }
              .map(_.map((_: Completed) => ControllerCommand.Response.Accepted))
              .runToFuture
        }

      case ControllerCommand.EmitTestEvent =>
        persist(ControllerTestEvent, async = true) { (_, updatedState) =>
          _controllerState = updatedState
          Right(ControllerCommand.Response.Accepted)
        }

      case ControllerCommand.ResetAgent(agentPath, force) =>
        agentRegister.checked(agentPath) match {
          case Left(problem) => Future.successful(Left(problem))
          case Right(agentEntry) =>
            journal.unsafeCurrentState().keyTo(AgentRefState).checked(agentEntry.agentPath) match {
              case Left(problem) => Future.successful(Left(problem))
              case Right(agentRefState) =>
                // TODO journal.lock(agentPath), to avoid race with AgentCoupled, too
                // As a workaround, AgentRefState.applyEvent ignores AgentCoupled if Resetting
                agentRefState.couplingState match {
                  case Resetting(frc) if !force || frc != force =>
                    Future.successful(Left(Problem.pure("AgentRef is already in state 'Resetting'")))
                  case reset: Reset if !force =>
                    Future.successful(Left(Problem.pure(s"AgentRef is already in state '$reset'")))
                  case _ =>
                    journal.unsafeCurrentState().resetAgent(agentPath, force = force) match {
                      case Left(problem) => Future.successful(Left(problem))
                      case Right(events) =>
                        journal.unsafeCurrentState().applyEvents(events) match {
                          case Left(problem) => Future.successful(Left(problem))
                          case Right(_) =>
                            persistTransactionAndSubsequentEvents(events) { (stampedEvents, updatedState) =>
                              // ResetAgent command may return with error despite it has reset the orders
                              agentEntry.isResetting = true
                              handleEvents(stampedEvents, updatedState)
                              agentEntry
                                .agentDriver.reset(force = force)
                                .tapError(t => Task(
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
                                .runToFuture
                            }.flatten
                       }
                    }
                }
            }
        }

      case ControllerCommand.ResetSubagent(subagentId, force) =>
        journal.unsafeCurrentState().keyTo(SubagentItemState).checked(subagentId)
          .map(subagentItemState =>
            (subagentItemState.couplingState != DelegateCouplingState.Resetting(force))
              .thenList((subagentId <-: SubagentResetStartedByController(force = force))))
          .match_ {
            case Left(problem) => Future.successful(Left(problem))
            case Right(events) =>
              persistMultiple(events) { (_, updated) =>
                _controllerState = updated
                proceedWithItem(subagentId)
              }.map(_ => Right(ControllerCommand.Response.Accepted))
          }

      case ControllerCommand.AnswerOrderPrompt(orderId) =>
        orderEventSource.answerPrompt(orderId) match {
          case Left(problem) =>
            Future.successful(Left(problem))
          case Right(events) =>
            persistTransactionAndSubsequentEvents(events)(handleEvents)
              .map(_ => Right(ControllerCommand.Response.Accepted))
        }

      case ControllerCommand.ClusterSwitchOver(Some(agentPath)) =>
        agentRegister.checked(agentPath)
          .map(_.agentDriver)
          .match_ {
            case Left(problem) => Future.successful(Left(problem))
            case Right(agentDriver) =>
              agentDriver
                .executeCommandDirectly(AgentCommand.ClusterSwitchOver)
                .logWhenItTakesLonger(s"$agentDriver.send(ClusterSwitchOver)")
                .materializeIntoChecked
                .rightAs(ControllerCommand.Response.Accepted)
                .runToFuture
          }

      case _ =>
        // Handled by ControllerCommandExecutor
        Future.failed(new NotImplementedError)
    }

  private def orderDeletedEvent(order: Order[Order.State]): KeyedEvent[OrderCoreEvent] =
    order.id <-: (
      if (order.isState[Order.IsTerminated])
        OrderDeleted
      else
        OrderDeletionMarked)

  private def executeOrderMarkCommands(orderIds: Vector[OrderId])
    (toEvents: OrderId => Checked[Option[List[OrderActorEvent]]])
  : Future[Checked[ControllerCommand.Response]] =
    if (!orderIds.areUnique)
      Future.successful(Left(Problem.pure("OrderIds must be unique")))
    else
      orderIds.traverse(_controllerState.idToOrder.checked) match {
        case Left(problem) =>
          Future.successful(Left(problem))

        case Right(orders) =>
          orders
            .flatTraverse(order => toEvents(order.id)
              .map(_.toVector.flatten.map(order.id <-: _)))
            .traverse(keyedEvents =>
              // Event may be inserted between events coming from Agent
              persistTransactionAndSubsequentEvents(keyedEvents)(handleEvents))
            .map(_.map(_ => ControllerCommand.Response.Accepted))
      }

  private def executeTransferOrders(cmd: TransferOrders)
  : Future[Checked[ControllerCommand.Response]] =
    new TransferOrderEventSource(_controllerState)
      .transferOrders(cmd)
      .match_ {
        case Left(problem) => Future.successful(Left(problem))
        case Right(events) =>
          persistTransaction(events ++ subsequentEvents(events)) { (stamped, updatedState) =>
            handleEvents(stamped, updatedState)
            Right(ControllerCommand.Response.Accepted)
          }
      }

  private def controlWorkflowPath(cmd: ControlWorkflowPath)
  : Future[Checked[ControllerCommand.Response]] =
    _controllerState.repo.pathToItems(Workflow).checked(cmd.workflowPath) match {
      case Left(problem) => Future.successful(Left(problem))
      case Right(_) =>
        val path = WorkflowPathControlPath(cmd.workflowPath)
        val (itemState, isNew) = _controllerState
          .pathToUnsignedSimple(WorkflowPathControl)
          .get(path) match {
            case None => WorkflowPathControl(path) -> true
            case Some(o) => o -> false
          }
        var item = itemState.item
        item = item.copy(
          suspended = cmd.suspend.fold(item.suspended)(identity),
          skip = item.skip
            -- cmd.skip.filterNot(_._2).keys
            ++ cmd.skip.filter(_._2).keys,
          itemRevision = Some(item.itemRevision.fold(ItemRevision.Initial)(_.next)))
        val event = if (isNew) UnsignedSimpleItemAdded(item) else UnsignedSimpleItemChanged(item)

        val keyedEvents = Vector(event)
          .concat(_controllerState.updatedWorkflowPathControlAttachedEvents(item))
          .map(NoKey <-: _)

        // Continue even if WorkflowPathControl is not changed.
        // This allows the caller to force the redistribution of the WorkflowPathControl.
        persistTransactionAndSubsequentEvents(keyedEvents) { (stamped, updated) =>
          handleEvents(stamped, updated)
          proceedWithItem(path)
          val workflowPathControl = updated.keyTo(WorkflowPathControl)(path)
          if (!workflowPathControl.item.suspended) {
            orderQueue.enqueue(
              updated.orders.filter(_.workflowPath == workflowPathControl.workflowPath).map(_.id))
          }
          Right(ControllerCommand.Response.Accepted)
        }
    }

  private def controlWorkflow(cmd: ControlWorkflow)
  : Future[Checked[ControllerCommand.Response]] =
    _controllerState.repo.idTo(Workflow)(cmd.workflowId) match {
      case Left(problem) => Future.successful(Left(problem))
      case Right(_) =>
        val workflowControlId = WorkflowControlId(cmd.workflowId)
        val (item0, isNew) = _controllerState
          .keyTo(WorkflowControl)
          .get(workflowControlId) match {
            case None => WorkflowControl(workflowControlId) -> true
            case Some(o) => o -> false
          }
        val item = item0.copy(
          breakpoints = item0.breakpoints -- cmd.removeBreakpoints ++ cmd.addBreakpoints,
          itemRevision = Some(item0.itemRevision.fold(ItemRevision.Initial)(_.next)))

        val event = if (isNew) UnsignedItemAdded(item) else UnsignedItemChanged(item)
        val keyedEvents = Vector(event)
          .concat(_controllerState.updatedWorkflowControlAttachedEvents(item))
          .map(NoKey <-: _)

        persistTransactionAndSubsequentEvents(keyedEvents) { (stamped, updated) =>
          handleEvents(stamped, updated)
          proceedWithItem(workflowControlId)
          Right(ControllerCommand.Response.Accepted)
        }
    }

  private def registerAgent(agent: AgentRef, eventId: EventId): AgentEntry = {
    val allocated = AgentDriver
      .resource(agent, eventId = eventId,
        (agentRunId, events) => Task.defer {
          val promise = Promise[Option[EventId]]()
          self ! Internal.EventsFromAgent(agent.path, agentRunId, events, promise)
          Task.fromFuture(promise.future)
        },
        orderIdToMarked => Task {
          self ! Internal.OrdersMarked(orderIdToMarked)
          // TODO Asynchronous ?
        },
        journal, agentDriverConfiguration, controllerConfiguration, context.system)
      .toAllocated
      .logWhenItTakesLonger("registerAgent")
      .awaitInfinite // TODO Blocking

    allocated.allocatedThing.untilStopped
      .*>(Task(
        self ! Internal.AgentDriverStopped(agent.path)))
      .runAsyncAndForget // TODO

    val entry = AgentEntry(agent, allocated)
    agentRegister.insert(agent.path, entry)
    entry
  }

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
    match {
      case Left(problem) => Future.successful(Left(problem))
      case Right(None) =>
        logger.debug(s"Discarding duplicate added Order: $freshOrder")
        Future.successful(Right(false))

      case Right(Some(orderAdded)) =>
        persistTransactionAndSubsequentEvents(orderAdded :: Nil) { (stamped, updatedState) =>
          handleEvents(stamped, updatedState)
          Right(true)
        }
        .flatMap(o => testAddOrderDelay.runToFuture.map(_ => o))  // test only
    }

  private def addOrders(freshOrders: Seq[FreshOrder]): Future[Checked[EventId]] =
    _controllerState.addOrders(freshOrders, suppressOrderIdCheckFor = suppressOrderIdCheckFor)
    match {
      case Left(problem) => Future.successful(Left(problem))
      case Right(events) =>
        persistTransaction(events) { (stamped, updatedState) =>
          handleEvents(stamped, updatedState)
          // Emit subsequent events later for earlier addOrders response (and smaller event chunk)
          orderQueue.enqueue(freshOrders.view.map(_.id))
          Right(updatedState.eventId)
        }
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
  : Unit = {
    val itemKeys = mutable.Set.empty[InventoryItemKey]
    val orderIds = mutable.Set.empty[OrderId]
    for (stamped <- stampedEvents) {
      val keyedEvent = stamped.value
      keyedEvent match {
        case KeyedEvent(orderId: OrderId, _: OrderEvent) =>
          orderIds += orderId
          orderIds ++= handleOrderEvent(keyedEvent.asInstanceOf[KeyedEvent[OrderEvent]])
          _controllerState = _controllerState.applyEvents(keyedEvent :: Nil).orThrow

        case KeyedEvent(_: NoKey, event: InventoryItemEvent) =>
          _controllerState = _controllerState.applyEvents(keyedEvent :: Nil).orThrow
          itemKeys += event.key
          handleItemEvent(event)

        case KeyedEvent(boardPath: BoardPath, NoticeDeleted(noticeId)) =>
          notices.deleteSchedule(boardPath, noticeId)
          _controllerState = _controllerState.applyEvents(keyedEvent :: Nil).orThrow

        case _ =>
          _controllerState = _controllerState.applyEvents(keyedEvent :: Nil).orThrow
      }
    }
    _controllerState = updatedState  // Reduce memory usage (they are equal)
    itemKeys foreach proceedWithItem
    proceedWithOrders(orderIds)
  }

  private def handleItemEvent(event: InventoryItemEvent): Unit = {
    event match {
      case UnsignedSimpleItemAdded(agentRef: AgentRef) =>
        registerAgent(agentRef, eventId = EventId.BeforeFirst)

        // TODO Not required in a future implementation, when Agents must be defined when referenced
        //reattachToAgent(agentRef.path)

      case UnsignedSimpleItemChanged(agentRef: AgentRef) =>
        agentRegister(agentRef.path) = agentRegister(agentRef.path).copy(agentRef = agentRef)
        val agentDriver = agentRegister(agentRef.path).agentDriver
        agentDriver
          .changeAgentRef(agentRef)
          .onErrorHandle(t => logger.error(
            s"$agentDriver.changeAgentRef => ${t.toStringWithCauses}", t.nullIfNoStackTrace))
          .logWhenItTakesLonger(s"$agentDriver.changeAgentRef")
          .awaitInfinite // TODO

      case UnsignedSimpleItemChanged(subagentItem: SubagentItem) =>
        for (agentRef <- journal.unsafeCurrentState().keyToItem(AgentRef).get(subagentItem.agentPath)) {
          val agentDriver = agentRegister(agentRef.path).agentDriver
          agentDriver
            .changeAgentRef(agentRef)
            .onErrorHandle(t => logger.error(
              s"$agentDriver.changeAgentRef => ${t.toStringWithCauses}", t.nullIfNoStackTrace))
            .logWhenItTakesLonger(s"$agentDriver.changeAgentRef")
            .awaitInfinite // TODO
        }

      case ItemDetached(itemKey, agentPath: AgentPath) =>
        for (agentEntry <- agentRegister.get(agentPath)) {
          agentEntry.detachingItems -= itemKey
        }

      case ItemDeleted(agentPath: AgentPath) =>
        for (entry <- agentRegister.get(agentPath)) {
          entry.isDeleted = true
          val agentDriver = entry.agentDriver
          agentDriver.terminate(reset = true)
            .onErrorHandle(t => logger.error(
              s"$agentDriver.terminate(reset) => ${t.toStringWithCauses}", t.nullIfNoStackTrace))
            .logWhenItTakesLonger(s"$agentDriver.terminate(reset)")
            .runAsyncAndForget // TODO
          // Actor terminates asynchronously, so do not add an AgentRef immediately after deletion!
        }

      case _ =>
    }
  }

  private def proceedWithItem(itemKey: InventoryItemKey): Unit = {
    itemKey match {
      case agentPath: AgentPath =>
        // TODO Handle AgentRef here: agentEntry .actor ! AgentDriver.Queueable.StartFetchingEvents ...

      case itemKey: InventoryItemKey =>
        for (agentToAttachedState <- _controllerState.itemToAgentToAttachedState.get(itemKey)) {
          for ((agentPath, attachedState) <- agentToAttachedState) {
            // TODO Does nothing if Agent is added later! (should be impossible, anyway)
            for (agentEntry <- agentRegister.get(agentPath)) {
              val agentDriver = agentEntry.agentDriver
              if (!agentEntry.isResetting) {
                attachedState match {
                  case Attachable =>
                    itemKey match {
                      case itemKey: SignableItemKey =>
                        for (signedItem <- _controllerState.keyToSignedItem.get(itemKey)) {
                          agentDriver
                            .send(AgentDriver.Queueable.AttachSignedItem(signedItem))
                            .onErrorHandle(t => logger.error(
                              s"$agentDriver.send(AttachSignedItem) => ${t.toStringWithCauses}", t.nullIfNoStackTrace))
                            .logAndIgnoreError(s"$agentDriver.send(AttachSignedItem)")
                            .awaitInfinite // TODO
                        }

                      case itemKey: UnsignedItemKey =>
                        for (item <- _controllerState.keyToItem.get(itemKey)) {
                          val unsignedItem = item.asInstanceOf[UnsignedItem]
                          agentDriver
                            .send(AgentDriver.Queueable.AttachUnsignedItem(unsignedItem))
                            .onErrorHandle(t => logger.error(
                              s"$agentDriver.send(AttachUnsignedItem) => ${t.toStringWithCauses}", t.nullIfNoStackTrace))
                            .logAndIgnoreError(s"$agentDriver.send(AttachUnsignedItem)")
                            .awaitInfinite // TODO
                        }
                    }

                  case Detachable =>
                    if (/*!agentEntry.isDeleted && */!agentEntry.detachingItems.contains(itemKey)) {
                      agentEntry.detachingItems += itemKey
                      agentDriver
                        .send(AgentDriver.Queueable.DetachItem(itemKey))
                        .onErrorHandle(t => logger.error(
                          s"$agentDriver.send(DetachItem) => ${t.toStringWithCauses}", t.nullIfNoStackTrace))
                        .logWhenItTakesLonger(s"$agentDriver.send(DetachItem)")
                        .awaitInfinite // TODO
                    }

                  case _ =>
                }
              }
            }
          }
        }

      case _ =>
    }

    // ResetSubagent
    itemKey match {
      case subagentId: SubagentId =>
        for (subagentItemState <- _controllerState.keyTo(SubagentItemState).get(subagentId)) {
          subagentItemState.isResettingForcibly match {
            case Some(force) =>
              for (agentDriver <- agentRegister.get(subagentItemState.item.agentPath).map(_.agentDriver)) {
                agentDriver
                  .send(AgentDriver.Queueable.ResetSubagent(subagentId, force))
                  .onErrorHandle(t => logger.error(
                    s"$agentDriver.send(ResetSubagent) => ${t.toStringWithCauses}", t.nullIfNoStackTrace))
                  .logWhenItTakesLonger(s"$agentDriver.send(ResetSubagent)")
                  .awaitInfinite // TODO
              }
            case _ =>
          }
        }
      case _ =>
    }
  }

  private def handleOrderEvent(keyedEvent: KeyedEvent[OrderEvent]): Set[OrderId] = {
    val KeyedEvent(orderId, event) = keyedEvent

    updateOrderEntry(orderId, event)

    event match {
      case _: OrderAdded =>
        Set.empty

      case _ =>
        _controllerState.idToOrder.get(orderId) match {
          case None =>
            logger.error(s"Unknown OrderId in event $keyedEvent")
            Set.empty

          case Some(order) =>
            val orderEventHandler = new OrderEventHandler(_controllerState.repo.idTo(Workflow))
            val checkedFollowUps = orderEventHandler.handleEvent(order, keyedEvent.event)
            val dependentOrderIds = mutable.Set.empty[OrderId]
            for (followUps <- checkedFollowUps.onProblem(p => logger.error(p))) {  // TODO OrderBroken on error?
              followUps foreach {
                case FollowUp.AddChild(childOrder) =>
                  dependentOrderIds += childOrder.id

                case FollowUp.Delete(deleteOrderId) =>
                  for (entry <- orderRegister.remove(deleteOrderId)) {
                    entry.timer.cancel()
                  }

                case _: FollowUp.LeaveJob =>
              }
            }

            event match {
              case OrderNoticePostedV2_3(notice) =>
                for (boardPath <- _controllerState.workflowPositionToBoardPath(order.workflowPosition)) {
                  notices.deleteSchedule(boardPath, notice.id)
                  notices.schedule(notice.toNotice(boardPath))
                }

              case OrderNoticePosted(notice) =>
                notices.deleteSchedule(notice.boardPath, notice.id)
                notices.schedule(notice)

              case _ =>
            }

            (dependentOrderIds.view ++
              (_controllerState.idToOrder.contains(orderId) ? order.id)
            ).toSet
        }
    }
  }

  private def updateOrderEntry(orderId: OrderId, event: OrderEvent): Unit = {
    val orderEntry = orderRegister.getOrElseUpdate(orderId, new OrderEntry(now))
    orderEntry.lastUpdatedAt = now
    event match {
      case _: OrderAttachable | _: OrderDetachable =>
        orderEntry.triedToAttached = false

      case OrderDetached =>
        orderEntry.isDetaching = false
        orderEntry.agentOrderMark = None

      case _ =>
    }
  }

  private def proceedWithOrders(orderIds: Iterable[OrderId]): Unit =
    if (!shuttingDown && switchover.isEmpty) {
      orderIds foreach proceedWithOrder
    }

  private def proceedWithOrder(orderId: OrderId): Unit =
    for (order <- _controllerState.idToOrder.get(orderId)) {
      if (order.isDetached) {
        for (until <- order.maybeDelayedUntil) {
          alarmClock.lock {
            if (until <= alarmClock.now()) {
              orderQueue.enqueue(orderId :: Nil)
            } else {
              for (entry <- orderRegister.get(orderId)) {
                // TODO Cancel timer when unused
                entry.timer := alarmClock.scheduleAt(until) {
                  self ! Internal.OrderIsDue(orderId)
                }
              }
            }
          }
        }
      }

      for (mark <- order.mark) {
        if (order.isAttached
          && !orderRegister.get(orderId).flatMap(_.agentOrderMark).contains(mark)) {
          // On Recovery, MarkOrder is sent again, because orderEntry.agentOrderMark is lost
          for ((_, agentEntry) <- checkedWorkflowAndAgentEntry(order)) {
            // CommandQueue filters multiple equal MarkOrder
            // because we may send multiple ones due to asynchronous execution
            val agentDriver = agentEntry.agentDriver
            agentDriver
              .send(AgentDriver.Queueable.MarkOrder(order.id, mark))
              .onErrorHandle(t => logger.error(
                s"$agentDriver.send(MarkOrder(${order.id})) => ${t.toStringWithCauses}", t.nullIfNoStackTrace))
              .logWhenItTakesLonger(s"$agentDriver.send(MarkOrder(${order.id}))")
              .awaitInfinite // TODO
          }
        }
      }

      order.attachedState match {
        case Some(_: Order.Attaching) =>
          for (order <- order.ifState[Order.IsFreshOrReady]) {
            tryAttachOrderToAgent(order)
          }

        case Some(_: Order.Detaching) =>
          detachOrderFromAgent(order.id)

        case _ =>
      }
    }

  private def delayOrderDeletion[E <: Event](keyedEvents: Seq[KeyedEvent[E]]): Seq[KeyedEvent[E]] =
    if (deleteOrderDelay.isZeroOrBelow)
      keyedEvents
    else
      keyedEvents.filter {
        case KeyedEvent(orderId: OrderId, OrderDeleted) =>
          orderRegister.get(orderId).fold(false) { orderEntry =>
            val delay = orderEntry.lastUpdatedAt + deleteOrderDelay - now
            !delay.isPositive || {
              orderRegister(orderId).timer := scheduler.scheduleOnce(delay) {
                self ! Internal.OrderIsDue(orderId)
              }
              false
            }
            // When recovering, proceedWithOrderOnController may emit the same event multiple times,
            // for example OrderJoined for each parent and child order.
            // These events are collected and with actor message Internal.AfterProceedEventsAdded reduced to one.
          }
        case _ => true
      }

  private def tryAttachOrderToAgent(order: Order[Order.IsFreshOrReady]): Unit =
    for ((signedWorkflow, agentEntry) <- checkedWorkflowAndAgentEntry(order)) {
      if (order.isAttaching && !agentEntry.isResetting) {
        val orderEntry = orderRegister(order.id)
        if (!orderEntry.triedToAttached) {
          val workflow = signedWorkflow.value
          import agentEntry.{agentDriver, agentPath}

          // Maybe attach Workflow
          val attachSignedItems: Seq[Task[Unit]] =
            workflow.referencedAttachableToAgentSignablePaths
              .flatMap(_controllerState.pathToSignedSimpleItem.get)
              .appended(signedWorkflow)
              .filter(signedItem => isDetachedOrAttachable(signedItem.value, agentPath))
              .map(signedItem =>
                agentDriver.send(AgentDriver.Queueable.AttachSignedItem(signedItem)))

          // Attach more required Items
          val attachUnsignedItems: Seq[Task[Unit]] =
            unsignedItemsToBeAttached(workflow, agentPath)
              .toVector
              .map(item =>
                agentDriver.send(AgentDriver.Queueable.AttachUnsignedItem(item)))

          // TODO AttachOrder mit parent orders!
          // Agent markiert die als bloß gebraucht für Kindaufträge
          // Mit Referenzzähler: der letzte Kindauftrag löscht seine Elternaufträge
          val attachOrder: Task[Unit] =
            agentDriver.send(AgentDriver.Queueable.AttachOrder(order, agentPath))

          orderEntry.triedToAttached = true

          // Now, Tasks are calculcated from mutable state and and be started as a sequence:
          (attachSignedItems ++ attachUnsignedItems :+ attachOrder)
            .sequence
            .map(_.combineAll)
            .onErrorHandle(t => logger.error(
              s"tryAttachOrderToAgent(${order.id}) => ${t.toStringWithCauses}", t.nullIfNoStackTrace))
            .logWhenItTakesLonger(s"tryAttachOrderToAgent(${order.id})")
            .awaitInfinite // TODO
        }
      }
    }

  private def unsignedItemsToBeAttached(workflow: Workflow, agentPath: AgentPath)
  : Iterable[UnsignedItem] = {
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

    // Workflow does not return those SubagentSelections which are referenced via
    // a variable expression.
    // So we attach all SubagentSelections which contain a SubagentId of the Agent
    result ++= _controllerState.keyToItem(SubagentSelection)
      .filter(_
        ._2.subagentIds
        .flatMap(_controllerState.keyToItem(SubagentItem).get)
        .exists(_.agentPath == agentPath))

    if (_controllerState.workflowToOrders.workflowIdToOrders contains workflow.id) {
      result ++= _controllerState.keyToItem(WorkflowPathControl)
        .get(WorkflowPathControlPath(workflow.path))
        .map(o => o.key -> o)
    }

    result ++= _controllerState.keyTo(WorkflowControl).get(WorkflowControlId(workflow.id))
      .map(o => o.key -> o)

    result.result().values.view
      .filter(isDetachedOrAttachable(_, agentPath))
  }

  private def checkedWorkflowAndAgentEntry(order: Order[Order.State])
  : Option[(Signed[Workflow], AgentEntry)] =
    order.attachedState match {
      case Some(Order.AttachedState.HasAgentPath(agentPath)) =>
        ( for {
            signedWorkflow <- _controllerState.repo.idToSigned(Workflow)(order.workflowId)
            agentEntry <- agentRegister.checked(agentPath)
          } yield (signedWorkflow, agentEntry)
        ).onProblem(p => logger.error(p.withPrefix("checkedWorkflowAndAgentEntry:")))

      case _ => None
    }

  private def isDetachedOrAttachable(item: InventoryItem, agentPath: AgentPath) = {
    val attachedState = _controllerState.itemToAttachedState(item.key, item.itemRevision, agentPath)
    attachedState == Detached || attachedState == Attachable
  }

  private def detachOrderFromAgent(orderId: OrderId): Unit =
    for (orderEntry <- orderRegister.get(orderId)) {
      if (!orderEntry.isDetaching) {
        _controllerState.idToOrder.checked(orderId)
          .flatMap(_.detaching)
          .onProblem(p => logger.error(s"detachOrderFromAgent '$orderId': not Detaching: $p"))
          .foreach { agentPath =>
            agentRegister.get(agentPath).map(_.agentDriver) match {
              case None => logger.error(s"detachOrderFromAgent '$orderId': Unknown $agentPath")
              case Some(agentDriver) =>
                orderEntry.isDetaching = true
                agentDriver
                  .send(AgentDriver.Queueable.DetachOrder(orderId))
                  .onErrorHandle(t => logger.error(
                    s"detachOrderFromAgent($orderId) => ${t.toStringWithCauses}", t.nullIfNoStackTrace))
                  .logWhenItTakesLonger(s"detachOrderFromAgent($orderId)")
                  .awaitInfinite // TODO
            }
          }
      }
    }

  private def instruction(workflowPosition: WorkflowPosition): Instruction =
    _controllerState.repo.idTo(Workflow)(workflowPosition.workflowId).orThrow
      .instruction(workflowPosition.position)

  private def clusterSwitchOver(restart: Boolean)
  : Future[Checked[ControllerCommand.Response.Accepted.type]] =
    if (switchover.isDefined)
      Future.successful(Left(Problem("Already switching over")))
    else
      Task {
        new Switchover(restart = restart)
      } .bracketCase { so =>
          switchover = Some(so)
          so.start()
            .materialize.flatTap {
              case Success(Right(_)) => Task.unit  // this.switchover is left for postStop
              case _ => Task {
                switchover = None  // Asynchronous!
              }
            }.dematerialize
        } ((so, exitCase) =>
          Task {
            logger.debug(s"SwitchOver => $exitCase")
            so.close()
          })
        .map(_.map((_: Completed) => ControllerCommand.Response.Accepted))
        .runToFuture

  private def orderEventSource = new OrderEventSource(_controllerState)

  private def runningAgentDriverCount =
    agentRegister.values.count(o => !o.actorTerminated)

  override def toString = "ControllerOrderKeeper"
}

private[controller] object ControllerOrderKeeper
{
  private val logger = Logger(getClass)

  object Input {
    final case object Start
  }

  sealed trait Command
  object Command {
    final case class Execute(command: ControllerCommand, meta: CommandMeta, correlId: CorrelId)
    extends Command
    final case class VerifiedUpdateItemsCmd(verifiedUpdateRepo: VerifiedUpdateItems) extends Command
  }

  private object Internal {
    case object ContinueWithNextOrderEvents extends DeadLetterSuppression
    final case class OrderIsDue(orderId: OrderId) extends DeadLetterSuppression
    final case class NoticeIsDue(boardPath: BoardPath, noticeId: NoticeId) extends DeadLetterSuppression
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
  }

  private implicit final class RichIdToOrder(private val idToOrder: Map[OrderId, Order[Order.State]])
  extends AnyVal {
    def checked(orderId: OrderId) = idToOrder.get(orderId).toChecked(UnknownOrderProblem(orderId))
  }

  private case class AgentEntry(
    agentRef: AgentRef,
    allocatedAgentDriver: Allocated[Task, AgentDriver])
  {
    var actorTerminated = false
    var isResetting = false
    var isDeleted = false
    val detachingItems = mutable.Set.empty[InventoryItemKey]

    def agentPath = agentRef.path

    val agentDriver: AgentDriver =
      allocatedAgentDriver.allocatedThing

    def release: Task[Unit] =
      allocatedAgentDriver.release
  }

  private class OrderEntry(now: MonixDeadline)
  {
    var triedToAttached = false
    var isDetaching = false
    var lastUpdatedAt: MonixDeadline = now
    var agentOrderMark = none[OrderMark]
    val timer = SerialCancelable()
  }

  object ControllerReadyTestIncident
}
