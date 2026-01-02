package js7.data.controller

import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.{RichIterable, RichIterableOnce}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.{isStrict, isTest}
import js7.data.Problems.AgentResetProblem
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.AgentResetStarted
import js7.data.board.NoticeEventSource
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventCalc, EventColl, KeyedEvent, TimeCtx}
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemAttachedStateEvent, ItemDeleted, ItemDetachable, ItemDetached}
import js7.data.item.ItemAttachedState.{Attachable, Attached, Detachable}
import js7.data.item.VersionedEvent.VersionedItemEvent
import js7.data.item.{InventoryItem, InventoryItemEvent, InventoryItemKey, SimpleItemPath}
import js7.data.job.JobResource
import js7.data.lock.LockState
import js7.data.order.OrderEvent.{OrderAddedEvent, OrderAddedEvents, OrderBroken, OrderCoreEvent, OrderDeleted, OrderExternalVanished, OrderForked, OrderLocksReleased, OrderMoved, OrderOrderAdded, OrderTransferred}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, OrderOutcome}
import js7.data.orderwatch.ExternalOrderKey
import js7.data.orderwatch.OrderWatchEvent.ExternalOrderRejected
import js7.data.plan.PlanFinishedEvent
import js7.data.subagent.SubagentItemState
import js7.data.subagent.SubagentItemStateEvent.SubagentReset
import js7.data.value.expression.scopes.NowScope
import js7.data.workflow.WorkflowControlId.syntax.*
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchPath, Position, PositionOrLabel}
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowControlId, WorkflowId, WorkflowPathControl, WorkflowPathControlPath}
import scala.annotation.tailrec
import scala.collection.immutable.{ArraySeq, VectorBuilder}
import scala.collection.{View, mutable}
import scala.language.implicitConversions

object ControllerStateExecutor:
  private val logger = Logger[this.type]

  // Same clock time for a chunk of operations
  private lazy val nowScope = NowScope()

  def addOrders(freshOrders: Seq[FreshOrder], suppressOrderIdCheckFor: Option[String] = None)
  : EventCalc[ControllerState, OrderAddedEvent, Any] =
    EventCalc.checked: controllerState =>
      freshOrders.checkUniquenessBy(_.id) *>
        freshOrders.flatTraverse:
          addOrder(controllerState, _, suppressOrderIdCheckFor = suppressOrderIdCheckFor)
            .map(_.toOption.toList.flatMap(_.toKeyedEvents))

  /** @return Right(Left(existingOrder)). */
  def addOrder(
    controllerState: ControllerState,
    order: FreshOrder,
    externalOrderKey: Option[ExternalOrderKey] = None,
    suppressOrderIdCheckFor: Option[String] = None)
  : Checked[Either[Order[Order.State], OrderAddedEvents]] =
    locally:
      if suppressOrderIdCheckFor.contains(order.id.string) then
        Checked.unit
      else
        order.id.checkedNameSyntax
    .productR:
      addOrderWithPrecheckedId(controllerState, order, externalOrderKey)

  private def addOrderWithPrecheckedId(
    controllerState: ControllerState,
    freshOrder: FreshOrder,
    externalOrderKey: Option[ExternalOrderKey])
  : Checked[Either[Order[Order.State], OrderAddedEvents]] =
    controllerState.idToOrder.get(freshOrder.id) match
      case Some(existing) =>
        // Ignore known orders — TODO Fail as duplicate if deleteWhenTerminated ?
        Checked(Left(existing))
      case None =>
        for
          workflow <- controllerState.repo.pathTo(Workflow)(freshOrder.workflowPath)
          preparedArguments <- workflow.orderParameterList.prepareOrderArguments(
            freshOrder, controllerState.controllerId, controllerState.keyToItem(JobResource), nowScope)
          _ <- controllerState.checkPlanAcceptsOrders(freshOrder.planId, allowClosedPlan = true)
          innerBlock <- workflow.nestedWorkflow(freshOrder.innerBlock)
          startPosition <- freshOrder.startPosition.traverse:
            checkStartAndStopPositionAndInnerBlock(_, workflow, freshOrder.innerBlock)
          _ <- freshOrder.stopPositions.toSeq.traverse:
            checkStartAndStopPositionAndInnerBlock(_, workflow, freshOrder.innerBlock)
          orderNoticeAnnounced <- NoticeEventSource
            .planToNoticeAnnounced(freshOrder.planId, freshOrder, innerBlock, controllerState)
            .map(_.map(freshOrder.id <-: _))
        yield
          Right:
            OrderAddedEvents(
              freshOrder.toOrderAdded(
                workflow.id.versionId, preparedArguments, externalOrderKey, startPosition),
              orderNoticeAnnounced)

  private def checkStartAndStopPositionAndInnerBlock(
    positionOrLabel: PositionOrLabel, workflow: Workflow, innerBlock: BranchPath)
  : Checked[Position] =
    for
      position <- workflow.positionOrLabelToPosition(positionOrLabel)
      _ <- position.isNestedIn(innerBlock) !! Problem(
        s"Position $position must be in innerBlock=${innerBlock.show}")
      _ <- workflow.checkPosition(position)
      _ <- workflow.isMoveable(innerBlock % 0, position) !! Problem(
        s"Order's startPosition or one of its stopPositions is not reachable: $positionOrLabel")
    yield
      position

  def startResetAgent(agentPath: AgentPath, force: Boolean): EventCalc[ControllerState, Event, TimeCtx] =
    EventCalc: coll =>
      for
        coll <- coll.add:
          agentPath <-: AgentResetStarted(force = force)
        coll <- coll.add:
          coll.aggregate.keyTo(SubagentItemState).values.view
            .filter(_.item.agentPath == agentPath)
            .map(_.path <-: SubagentReset)
        coll <- coll.add:
          coll.aggregate.idToOrder.values.view
            .filter(_.isAtAgent(agentPath))
            .map: order =>
              forciblyDetachOrder(order, agentPath)
            .toVector.combineAll
        coll <- coll.add:
          coll.aggregate.itemToAgentToAttachedState.to(View)
            .filter(_._2.contains(agentPath))
            .map(_._1)
            .map: itemKey =>
              NoKey <-: ItemDetached(itemKey, agentPath)
      yield
        coll

  private def forciblyDetachOrder(order: Order[Order.State], agentPath: AgentPath)
  : EventCalc[ControllerState, OrderCoreEvent, TimeCtx] =
    EventCalc.checked: controllerState =>
      given InstructionExecutorService = InstructionExecutorService(EventCalc.clock)
      val outcome = OrderOutcome.Disrupted(AgentResetProblem(agentPath))
      for
        orderEvents <- order.forceDetach(outcome).map(_._1).map(_.map(order.id <-: _))
        detachedState <- controllerState.applyKeyedEvents(orderEvents)
        fail <- OrderEventSource(detachedState)
          .fail(detachedState.idToOrder(order.id), Some(outcome), uncatchable = true)
      yield
        orderEvents ++ fail.view.map(order.id <-: _)

  def addSubsequentEvents(coll: EventColl[ControllerState, Event, TimeCtx])
  : Checked[EventColl[ControllerState, Event, TimeCtx]] =
    addSubsequentEvents(coll, 0)

  private def addSubsequentEvents(coll: EventColl[ControllerState, Event, TimeCtx], recursion: Int)
  : Checked[EventColl[ControllerState, Event, TimeCtx]] =
    directSubsequentEvents(coll) match
      case Left(problem) => Left(problem)
      case Right(subcoll) =>
        if !subcoll.hasEvents then
          Right(coll)
        else if isStrict && recursion > 10 && coll.keyedEvents.endsWith(subcoll.keyedEvents) then
          subcoll.foreachLog(o => logger.error(s"addSubsequentEvents recursion=$recursion $o"))
          throw new AssertionError("addSubsequentEvents endless recursion")
        else
          // TODO Avoid recursion
          addSubsequentEvents(subcoll, recursion + 1) match
            case Left(problem) => Left(problem)
            case Right(subcoll2) => coll.add(subcoll2)

  /** Returns the directly subsequent events. */
  def directSubsequentEvents(coll: EventColl[ControllerState, Event, TimeCtx])
  : Checked[EventColl[ControllerState, Event, TimeCtx]] =
    // (looks like an EventCalc, but has a different semantic)
    eventsToTouchedThings(coll).flatMap: tuple =>
      touchedThingsToSubsequentEvents.tupled(tuple).calculate(coll)

  private def eventsToTouchedThings(coll: EventColl[ControllerState, Event, TimeCtx])
  : Checked[(
    collection.Set[InventoryItemKey],
    collection.Set[OrderId],
    collection.Set[WorkflowId],
    collection.Set[InventoryItemKey],
    collection.Seq[(WorkflowId, AgentPath)],
    collection.Seq[WorkflowId])
  ] =
    val touchedItemKeys = mutable.Set.empty[InventoryItemKey]
    val touchedOrderIds = mutable.Set.empty[OrderId]
    val detachWorkflowCandidates = mutable.Set.empty[WorkflowId]
    val detachedItems = mutable.Set.empty[InventoryItemKey]
    val detachedWorkflows = mutable.Buffer.empty[(WorkflowId, AgentPath)]
    val deletedWorkflows = mutable.Buffer.empty[WorkflowId]

    // TODO Avoid reapply events?
    var controllerState = coll.originalAggregate
    var checked: Checked[Unit] = Checked.unit
    val iterator = coll.keyedEvents.iterator

    while iterator.hasNext && checked.isRight do
      val keyedEvent = iterator.next()
      val previous = controllerState

      controllerState.applyKeyedEvent(keyedEvent) match
        case Left(problem) =>
          checked = Left(problem)

        case Right(updated) =>
          controllerState = updated
          keyedEvent match
            case KeyedEvent(_, event: VersionedItemEvent) =>
              for previousItem <- previous.repo.pathToVersionedItem(event.path) do
                touchedItemKeys += previousItem.id
                previousItem match
                  case previousItem: Workflow =>
                    detachWorkflowCandidates += previousItem.id
                  case _ =>

            case KeyedEvent(_: NoKey, event: InventoryItemEvent) =>
              touchedItemKeys += event.key

              event match
                case ItemDetached(itemKey, agentPath: AgentPath) =>
                  detachedItems += itemKey
                  itemKey match
                    case WorkflowId.as(workflowId: WorkflowId) =>
                      detachedWorkflows += workflowId -> agentPath
                    case _ =>

                case ItemDeleted(WorkflowId.as(workflowId)) =>
                  deletedWorkflows += workflowId

                case _ =>

            case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
              touchedOrderIds += orderId
              touchedOrderIds ++= keyedEventToPendingOrderIds(controllerState, orderId <-: event)
              event match
                case OrderDeleted | _: OrderTransferred =>
                  detachWorkflowCandidates += previous.idToOrder(orderId).workflowId
                case _ =>

            case _ =>

    for _ <- checked yield
      (touchedItemKeys, touchedOrderIds, detachWorkflowCandidates, detachedItems, detachedWorkflows,
        deletedWorkflows)

  private def touchedThingsToSubsequentEvents(
    touchedItemKeys: collection.Set[InventoryItemKey],
    touchedOrderIds: collection.Set[OrderId],
    detachWorkflowCandidates: collection.Set[WorkflowId],
    detachedItems1: collection.Set[InventoryItemKey],
    detachedWorkflows: collection.Seq[(WorkflowId, AgentPath)],
    deletedWorkflows: collection.Seq[WorkflowId])
  : EventCalc[ControllerState, Event, TimeCtx] =
    EventCalc: coll =>
      val detachedItems = mutable.Set.empty[InventoryItemKey] ++ detachedItems1
      for
        coll <- Right(coll.forward)
        coll <- coll.add:
          nextItemAttachedStateEvents(touchedItemKeys)
        coll <- coll.add:
          detachWorkflowCandidates.view
            .filter(coll.aggregate.keyToItem.contains)
            .filter(coll.aggregate.isObsoleteItem)
            .flatMap: workflowId =>
              coll.aggregate.itemToAgentToAttachedState
                .get(workflowId).view
                .flatMap(_.collect:
                  // Workflows are never Attachable, only Attached or Detachable
                  case (agentPath, Attached(_)) =>
                    detachedItems += workflowId
                    NoKey <-: ItemDetachable(workflowId, agentPath))
        coll <- coll.add:
          detachedWorkflows.view
            .flatMap: (workflowId, agentPath) =>
              val workflowControlId = WorkflowControlId(workflowId)
              coll.aggregate.itemToAgentToAttachedState.get(workflowControlId)
                .flatMap(_.get(agentPath))
                .exists(_.isAttachableOrAttached)
                .thenSome:
                  NoKey <-: ItemDetachable(workflowControlId, agentPath)
        coll <- coll.add:
          (detachWorkflowCandidates.view ++ detachedItems)
            .filter(coll.aggregate.keyToItem.contains)
            .filterNot(coll.aggregate.itemToAgentToAttachedState.contains)
            .filter:
              case WorkflowId.as(workflowId) =>
                coll.aggregate.isObsoleteItem(workflowId)

              case path: WorkflowPathControlPath =>
                // Delete when no Workflow version exists
                !coll.aggregate.repo.pathToItems(Workflow).contains(path.workflowPath)

              case WorkflowControlId.as(workflowControlId) =>
                // Delete when Workflow no longer exists
                !coll.aggregate.idToWorkflow.isDefinedAt(workflowControlId.workflowPath)

              case path: SimpleItemPath =>
                !coll.aggregate.isReferenced(path)

              case _ =>
                false
            .map: itemKey =>
              NoKey <-: ItemDeleted(itemKey)
            .toSet
        coll <- coll.add:
          detachedItems.view
            .filterNot(coll.aggregate.keyToItem.contains)
            .filter:
              case path: WorkflowPathControlPath =>
                // Delete when no Workflow version exists
                !coll.aggregate.repo.pathToItems(Workflow).contains(path.workflowPath)

              case WorkflowControlId.as(workflowControlId) =>
                // Delete when Workflow no longer exists
                !coll.aggregate.idToWorkflow.isDefinedAt(workflowControlId.workflowPath)

              case _ =>
                false
            .map: itemKey =>
              NoKey <-: ItemDeleted(itemKey)
        coll <- coll.add:
          deletedWorkflows.view
            .filterNot(coll.aggregate.keyToItem.contains)
            .flatMap:
              case WorkflowId.as(workflowId) =>
                // Delete when no Workflow version exists
                val deleteWorkflowPathControl =
                  val workflowPathControlPath = WorkflowPathControlPath(workflowId.path)
                  (coll.aggregate.keyTo(WorkflowPathControl).contains(workflowPathControlPath) &&
                    !coll.aggregate.itemToAgentToAttachedState.contains(workflowPathControlPath) &&
                    !coll.aggregate.repo.pathToItems(Workflow).contains(workflowId.path)
                  ) ? (NoKey <-: ItemDeleted(workflowPathControlPath))

                // Delete WorkflowControl when Workflow no longer exists
                val deleteWorkflowControl =
                  val workflowControlId = WorkflowControlId(workflowId)
                  (coll.aggregate.keyTo(WorkflowControl).contains(workflowControlId) &&
                    !coll.aggregate.itemToAgentToAttachedState.contains(workflowControlId) &&
                    !coll.aggregate.idToWorkflow.isDefinedAt(workflowControlId.workflowPath)
                  ) ? (NoKey <-: ItemDeleted(workflowControlId))

                deleteWorkflowPathControl ++ deleteWorkflowControl

              case _ =>
                Nil
        coll <- coll.add:
          nextOrderEvents(touchedOrderIds)
        coll <- coll.add:
          nextOrderWatchOrderEvents(coll.aggregate)
      yield
        coll

  private def nextItemAttachedStateEvents(itemKeys: Iterable[InventoryItemKey])
  : EventCalc[ControllerState, ItemAttachedStateEvent, Any] =
    EventCalc: coll =>
      coll.addEventCalc:
        itemKeys.view
          .flatMap(coll.aggregate.keyToItem.get)
          .map(nextItemAttachedStateEventsForItem)
          .foldMonoids

  private def nextItemAttachedStateEventsForItem(item: InventoryItem)
  : EventCalc[ControllerState, ItemAttachedStateEvent, Any] =
    EventCalc: coll =>
      coll.aggregate.itemToAgentToAttachedState.get(item.key) match
        case Some(agentPathToAttached) =>
          coll.iterate(agentPathToAttached):
            case (coll, (agentPath, Attached(revision))) =>
              def detachEvent = detach(coll.aggregate, item.key, agentPath)

              item.key match
                case WorkflowId.as(workflowId) =>
                  if coll.aggregate.isObsoleteItem(workflowId) then
                    for
                      coll <- coll.add:
                        derivedWorkflowPathControlEvent(workflowId, agentPath)
                      coll <- coll.add:
                        derivedWorkflowControlEvent(workflowId, agentPath)
                      coll <- coll.add:
                        detachEvent
                    yield
                      coll
                  else
                    Right(coll)

                case itemKey =>
                  if coll.aggregate.deletionMarkedItems contains itemKey then
                    coll.add:
                      detachEvent
                  else
                    coll.add:
                      item.itemRevision != revision thenSome:
                        item.dedicatedAgentPath match
                          case Some(`agentPath`) | None =>
                            // Item is dedicated to this Agent or is required by an Order.
                            // Attach again without detaching, and let Agent change the item while in flight
                            NoKey <-: ItemAttachable(itemKey, agentPath)
                          case Some(_) =>
                            // Item's Agent dedication has changed, so we detach it
                            detachEvent

            case (coll, (_, Attachable | Detachable)) =>
              Right(coll)

        case None =>
          if coll.aggregate.deletionMarkedItems.contains(item.key) then
            Right(coll)
          else
            coll.add:
              item.dedicatedAgentPath.map(NoKey <-: ItemAttachable(item.key, _))

  private def derivedWorkflowPathControlEvent(workflowId: WorkflowId, agentPath: AgentPath)
  : EventCalc[ControllerState, ItemAttachedStateEvent, Any] =
    EventCalc.maybe: controllerState =>
      // Implicitly detach WorkflowPathControl from agentPath
      // when the last version of WorkflowPath has been detached
      val otherWorkflowWillStillBeAttached = controllerState.repo
        .pathToItems(Workflow)(workflowId.path)
        .map(_.id)
        .filter(_ != workflowId) // workflowId is going to be detached
        .exists(workflowId => controllerState
          .itemToAgentToAttachedState.get(workflowId)
          .exists(_ contains agentPath))

      !otherWorkflowWillStillBeAttached thenMaybe:
        val controlPath = WorkflowPathControlPath(workflowId.path)
        controllerState.itemToAgentToAttachedState
          .get(controlPath)
          .flatMap(_
            .get(agentPath)
            .collect:
              case Attachable | Attached(_) => detach(controllerState, controlPath, agentPath))

  private def derivedWorkflowControlEvent(workflowId: WorkflowId, agentPath: AgentPath)
  : EventCalc[ControllerState, ItemAttachedStateEvent, Any] =
    EventCalc.multiple: controllerState =>
      // Implicitly detach WorkflowControl from agentPath when Workflow has been detached
      if controllerState.itemToAgentToAttachedState.contains(workflowId) then
        Nil
      else
        val controlId = WorkflowControlId(workflowId)
        controllerState.itemToAgentToAttachedState
          .get(controlId)
          .flatMap(_
            .get(agentPath)
            .collect:
              case Attachable | Attached(_) => detach(controllerState, controlId, agentPath))

  private def detach(
    controllerState: ControllerState,
    itemKey: InventoryItemKey,
    agentPath: AgentPath)
  : KeyedEvent[ItemAttachedStateEvent] =
    if controllerState.keyToUnsignedItemState_.contains(agentPath) then
      NoKey <-: ItemDetachable(itemKey, agentPath)
    else
      // shortcut in case the Agent has been deleted (reset)
      NoKey <-: ItemDetached(itemKey, agentPath)

  def updatedWorkflowPathControlAttachedEvents(workflowPathControl: WorkflowPathControl)
  : EventCalc[ControllerState, ItemAttachable, Any] =
    EventCalc.multiple: controllerState =>
      controllerState
        .repo.pathToItems(Workflow)
        .getOrElse(workflowPathControl.workflowPath, View.empty)
        .view
        .flatMap: workflow =>
          controllerState.itemToAgentToAttachedState
            .getOrElse(workflow.id, Map.empty)
            .collect:
              case (agentPath, Attachable | Attached(_)) => agentPath
        .toSet
        .map(ItemAttachable(workflowPathControl.path, _))

  def updatedWorkflowControlAttachedEvents(controllerState: ControllerState, workflowControl: WorkflowControl)
  : Iterable[ItemAttachable] =
    controllerState
      .repo.idTo(Workflow)(workflowControl.workflowId)
      .toOption.view
      .flatMap: workflow =>
        controllerState.itemToAgentToAttachedState
          .getOrElse(workflow.id, Map.empty)
          .collect:
            case (agentPath, Attachable | Attached(_)) => agentPath
      .toSet
      .map:
        ItemAttachable(workflowControl.id, _)

  def nextOrderWatchOrderEvents(controllerState: ControllerState)
  : View[KeyedEvent[OrderAddedEvent | ExternalOrderRejected | OrderExternalVanished]] =
    controllerState.ow.nextEvents(addOrder(controllerState, _, _))

  // TODO Try to call with only relevant OrderIDs
  //  For example, don't call with all orders waiting for a single-order lock.
  def nextOrderEvents(orderIds: Iterable[OrderId]): EventCalc[ControllerState, Event, TimeCtx] =
    EventCalc.multiple: controllerState_ =>
      var controllerState = controllerState_
      val _keyedEvents = new VectorBuilder[KeyedEvent[OrderCoreEvent | PlanFinishedEvent]]
      var nextIndex = 0L

      final case class Entry(orderId: OrderId, priority: BigDecimal, index: Long)
      extends Ordered[Entry]:
        def compare(o: Entry): Int =
          priority compare o.priority match
            case 0 => o.index compare index // Reverse, because early index has higher priority
            case n => n

      def toEntry(orderId: OrderId): Option[Entry] =
        controllerState.idToOrder.get(orderId).map: order =>
          val i = nextIndex
          nextIndex += 1
          Entry(orderId, order.priority, i)

      val queue = mutable.PriorityQueue.from(orderIds.view.flatMap(toEntry))

      @tailrec def loop(): Unit =
        if queue.nonEmpty then
          val orderId = queue.dequeue().orderId
          if controllerState.idToOrder contains orderId then {
            given InstructionExecutorService = InstructionExecutorService(EventCalc.clock)
            val keyedEvents = OrderEventSource(controllerState).nextEvents(orderId)
            if keyedEvents.nonEmpty then
              for case KeyedEvent(orderId, OrderBroken(maybeProblem)) <- keyedEvents do
                logger.error(s"$orderId is broken${maybeProblem.fold("")(": " + _)}") // ???
              controllerState.applyKeyedEvents(keyedEvents) match
                case Left(problem) =>
                  logger.error(s"$orderId: $problem")  // Should not happen
                case Right(state) =>
                  controllerState = state
                  _keyedEvents ++= keyedEvents
                  queue ++= keyedEvents.view
                    .collect:
                      case e @ KeyedEvent(_, _: OrderEvent) => e.asInstanceOf[KeyedEvent[OrderEvent]]
                    .flatMap(keyedEventToPendingOrderIds(controllerState, _))
                    .toSet.view.flatMap(toEntry)

                  //<editor-fold desc="if isTest ...">
                  if isTest && _keyedEvents.size >= 1_000_000 then
                    for (ke, i) <- _keyedEvents.result().view.take(1000).zipWithIndex do
                      logger.error(s"$i: $ke")
                    throw new AssertionError(
                      s"🔥 ${_keyedEvents.size} events generated, probably a loop")
                  end if
                  //</editor-fold>
          }
          loop()

      loop()
      _keyedEvents.result()

  private def optimizeKeyedEvents[E <: Event](keyedEvents: Seq[KeyedEvent[E]]): Seq[KeyedEvent[E]] =
    val orderToIndex = mutable.Map[OrderId, Int]()
    val buffer = mutable.ArrayBuffer[KeyedEvent[E]]()
    buffer.sizeHint(keyedEvents.size)
    keyedEvents.foreach:
      case ke @ KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        def add() =
          orderToIndex(orderId) = buffer.length
          buffer += ke
        event match
          case OrderMoved(_, None) =>
            orderToIndex.get(orderId) match
              case None => add()
              case Some(i) =>
                buffer(i) match
                  case KeyedEvent(_, OrderMoved(_, None)) =>
                    buffer(i) = ke
                  case _ => add()

          case _ => add()

      case ke =>
        buffer += ke
    buffer.to(ArraySeq)

  private def keyedEventToPendingOrderIds(
    controllerState: ControllerState,
    keyedEvent: KeyedEvent[OrderEvent])
  : View[OrderId] =
    keyedEvent.key +: keyedEvent.event.match
      case OrderLocksReleased(lockPaths) =>
        lockPaths.view
          .flatMap(controllerState.keyTo(LockState).get)
          .flatMap(_.queue)

      case OrderForked(children) =>
        children.view.map(_.orderId)

      case orderOrderAdded: OrderOrderAdded =>
        View.Single(orderOrderAdded.orderId)

      case _ => View.empty
