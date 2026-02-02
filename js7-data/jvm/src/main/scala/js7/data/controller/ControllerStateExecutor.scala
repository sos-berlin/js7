package js7.data.controller

import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits.{RichIterable, RichIterableOnce}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict
import js7.data.Problems.AgentResetProblem
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.AgentResetStarted
import js7.data.board.NoticeEventSource
import js7.data.controller.ControllerEventCalc
import js7.data.event.EventColl.extensions.now
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, EventCalc, EventCalcCtx, EventColl, KeyedEvent}
import js7.data.execution.workflow.OrderEventSource
import js7.data.state.EngineStateExtensions.keyedEventToPendingOrderIds
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemAttachedStateEvent, ItemDeleted, ItemDetachable, ItemDetached}
import js7.data.item.ItemAttachedState.{Attachable, Attached, Detachable}
import js7.data.item.VersionedEvent.VersionedItemEvent
import js7.data.item.{InventoryItem, InventoryItemEvent, InventoryItemKey, SimpleItemPath}
import js7.data.job.JobResource
import js7.data.order.OrderEvent.{OrderAddedEvent, OrderAddedEvents, OrderAwoke, OrderDeleted, OrderDetached, OrderExternalVanished, OrderProcessed, OrderTransferred}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, OrderOutcome}
import js7.data.orderwatch.ExternalOrderKey
import js7.data.orderwatch.OrderWatchEvent.ExternalOrderRejected
import js7.data.state.EngineEventColl.extensions.order
import js7.data.subagent.SubagentItemState
import js7.data.subagent.SubagentItemStateEvent.SubagentReset
import js7.data.value.expression.scopes.NowScope
import js7.data.workflow.WorkflowControlId.syntax.*
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchPath, Position, PositionOrLabel}
import js7.data.workflow.{Workflow, WorkflowControl, WorkflowControlId, WorkflowId, WorkflowPathControl, WorkflowPathControlPath}
import scala.collection.{View, mutable}
import scala.language.implicitConversions

object ControllerStateExecutor:
  private val logger = Logger[this.type]

  // Same clock time for a chunk of operations

  def addOrders(freshOrders: Seq[FreshOrder], suppressOrderIdCheckFor: Option[String] = None)
  : EventCalc[ControllerState, OrderAddedEvent] =
    EventCalc: coll =>
      coll:
        freshOrders.checkUniquenessBy(_.id) *>
          freshOrders.flatTraverse:
            addOrder(coll.aggregate, _, coll.now, suppressOrderIdCheckFor = suppressOrderIdCheckFor)
              .map(_.toOption.toList.flatMap(_.toKeyedEvents))

  /** @return Right(Left(existingOrder)). */
  def addOrder(
    controllerState: ControllerState,
    order: FreshOrder,
    now: Timestamp,
    externalOrderKey: Option[ExternalOrderKey] = None,
    suppressOrderIdCheckFor: Option[String] = None)
  : Checked[Either[Order[Order.State], OrderAddedEvents]] =
    locally:
      if suppressOrderIdCheckFor.contains(order.id.string) then
        Checked.unit
      else
        order.id.checkedNameSyntax
    .productR:
      addOrderWithPrecheckedId(controllerState, order, externalOrderKey, now)

  private def addOrderWithPrecheckedId(
    controllerState: ControllerState,
    freshOrder: FreshOrder,
    externalOrderKey: Option[ExternalOrderKey],
    now: Timestamp)
  : Checked[Either[Order[Order.State], OrderAddedEvents]] =
    controllerState.idToOrder.get(freshOrder.id) match
      case Some(existing) =>
        // Ignore known orders — TODO Fail as duplicate if deleteWhenTerminated ?
        Checked(Left(existing))
      case None =>
        for
          workflow <- controllerState.repo.pathTo(Workflow)(freshOrder.workflowPath)
          preparedArguments <- workflow.orderParameterList.prepareOrderArguments(
            freshOrder, controllerState.controllerId, controllerState.keyToItem(JobResource),
            NowScope(now))
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

  def startResetAgent(agentPath: AgentPath, force: Boolean): EventCalc[ControllerState, Event] =
    EventCalc: coll =>
      for
        coll <- coll:
          agentPath <-: AgentResetStarted(force = force)
        coll <- coll:
          coll.aggregate.keyTo(SubagentItemState).values.view
            .filter(_.item.agentPath == agentPath)
            .map(_.path <-: SubagentReset)
        coll <- coll:
          coll.aggregate.idToOrder.values.view
            .filter(_.isAtAgent(agentPath))
            .map: order =>
              forciblyDetachOrder(order.id, agentPath)
            .toVector.combineAll
        coll <- coll:
          coll.aggregate.itemToAgentToAttachedState.to(View)
            .filter(_._2.contains(agentPath))
            .map(_._1)
            .map: itemKey =>
              NoKey <-: ItemDetached(itemKey, agentPath)
      yield
        coll

  private def forciblyDetachOrder(orderId: OrderId, agentPath: AgentPath): ControllerEventCalc =
    EventCalc: coll =>
      val outcome = OrderOutcome.Disrupted(AgentResetProblem(agentPath))
      for
        order <- coll.order(orderId)
        coll <- coll:
          order.ifState[Order.Processing].map: _ =>
            orderId <-: OrderProcessed(outcome)
        coll <- coll:
          orderId <-: OrderDetached
        coll <- coll.addWithKey(order.id):
          order.resetState // Reset state to allow failing the order
        order <- coll.order(orderId)
        coll <- coll:
          order.ifState[Order.DelayedAfterError].map: _ =>
            orderId <-: OrderAwoke
        coll <- coll:
          OrderEventSource.fail(orderId, Some(outcome), uncatchable = true)
      yield
        coll

  def addSubsequentEvents(coll: EventColl[ControllerState, Event])
  : Checked[EventColl[ControllerState, Event]] =
    addSubsequentEvents(coll, 0)

  private def addSubsequentEvents(coll: EventColl[ControllerState, Event], recursion: Int)
  : Checked[EventColl[ControllerState, Event]] =
    directSubsequentEvents(coll) match
      case Left(problem) => Left(problem)
      case Right(subcoll) =>
        if !subcoll.hasEvents then
          Right(coll)
        else if isStrict && recursion > 10 && coll.keyedEvents.endsWith(subcoll.keyedEvents) then
          subcoll.foreachEventString(o => logger.error(s"addSubsequentEvents recursion=$recursion $o"))
          throw new AssertionError("addSubsequentEvents endless recursion")
        else
          // TODO Avoid recursion
          addSubsequentEvents(subcoll, recursion + 1) match
            case Left(problem) => Left(problem)
            case Right(subcoll2) => coll.add(subcoll2)

  /** Returns the directly subsequent events. */
  def directSubsequentEvents(coll: EventColl[ControllerState, Event])
  : Checked[EventColl[ControllerState, Event]] =
    // (looks like an EventCalc, but has a different semantic)
    eventsToTouchedThings(coll).flatMap: tuple =>
      coll.addEventCalc:
        touchedThingsToSubsequentEvents.tupled(tuple)

  private def eventsToTouchedThings(coll: EventColl[ControllerState, Event])
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
              touchedOrderIds ++= controllerState.keyedEventToPendingOrderIds(orderId <-: event)
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
  : EventCalc[ControllerState, Event] =
    EventCalc: coll =>
      val detachedItems = mutable.Set.empty[InventoryItemKey] ++ detachedItems1
      for
        coll <- Right(coll.forward)
        coll <-
          if touchedItemKeys.isEmpty then
            coll.nix
          else
            coll(nextItemAttachedStateEvents(touchedItemKeys))
        coll <- coll:
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
        coll <- coll:
          detachedWorkflows.view
            .flatMap: (workflowId, agentPath) =>
              val workflowControlId = WorkflowControlId(workflowId)
              coll.aggregate.itemToAgentToAttachedState.get(workflowControlId)
                .flatMap(_.get(agentPath))
                .exists(_.isAttachableOrAttached)
                .thenSome:
                  NoKey <-: ItemDetachable(workflowControlId, agentPath)
        coll <- coll:
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
        coll <- coll:
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
        coll <- coll:
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
        coll <- coll:
          nextOrderEvents(touchedOrderIds)
        coll <- coll:
          nextOrderWatchOrderEvents(coll.aggregate, coll.now)
      yield
        coll

  private def nextItemAttachedStateEvents(itemKeys: Iterable[InventoryItemKey])
  : EventCalc[ControllerState, ItemAttachedStateEvent] =
    EventCalc: coll =>
      coll.addEventCalc:
        itemKeys.view
          .flatMap(coll.aggregate.keyToItem.get)
          .map(nextItemAttachedStateEventsForItem)
          .foldMonoids

  private def nextItemAttachedStateEventsForItem(item: InventoryItem)
  : EventCalc[ControllerState, ItemAttachedStateEvent] =
    EventCalc: coll =>
      coll.aggregate.itemToAgentToAttachedState.get(item.key) match
        case Some(agentPathToAttached) =>
          coll.fold(agentPathToAttached):
            case (coll, (agentPath, Attached(revision))) =>
              def detachEvent = detach(coll.aggregate, item.key, agentPath)

              item.key match
                case WorkflowId.as(workflowId) =>
                  if coll.aggregate.isObsoleteItem(workflowId) then
                    for
                      coll <- coll:
                        derivedWorkflowPathControlEvent(workflowId, agentPath)
                      coll <- coll:
                        derivedWorkflowControlEvent(workflowId, agentPath)
                      coll <- coll:
                        detachEvent
                    yield
                      coll
                  else
                    Right(coll)

                case itemKey =>
                  if coll.aggregate.deletionMarkedItems contains itemKey then
                    coll:
                      detachEvent
                  else
                    coll:
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
            coll:
              item.dedicatedAgentPath.map(NoKey <-: ItemAttachable(item.key, _))

  private def derivedWorkflowPathControlEvent(workflowId: WorkflowId, agentPath: AgentPath)
  : EventCalc[ControllerState, ItemAttachedStateEvent] =
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
  : EventCalc[ControllerState, ItemAttachedStateEvent] =
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
  : EventCalc[ControllerState, ItemAttachable] =
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

  def nextOrderWatchOrderEvents(controllerState: ControllerState, now: Timestamp)
  : View[KeyedEvent[OrderAddedEvent | ExternalOrderRejected | OrderExternalVanished]] =
    controllerState.ow.nextEvents: (order: FreshOrder, ext: Option[ExternalOrderKey]) =>
      addOrder(controllerState, order, now, ext)

  // TODO Try to call with only relevant OrderIds
  //  For example, don't call with all orders waiting for a single-order lock.
  def nextOrderEvents(orderIds: Iterable[OrderId]): ControllerEventCalc =
    OrderEventSource.nextEvents[ControllerState](orderIds).widen
