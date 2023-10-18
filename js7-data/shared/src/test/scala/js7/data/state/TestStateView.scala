package js7.data.state

import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.NotImplementedMap
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.BoardPath
import js7.data.calendar.CalendarPath
import js7.data.controller.ControllerId
import js7.data.event.{Event, EventDrivenState, KeyedEvent}
import js7.data.item.{InventoryItem, InventoryItemKey, UnsignedItemKey, UnsignedItemState, UnsignedSimpleItemPath, UnsignedSimpleItemState}
import js7.data.lock.LockPath
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import scala.collection.MapView

case class TestStateView(
  isAgent: Boolean,
  controllerId: ControllerId = ControllerId("CONTROLLER"),
  idToOrder: Map[OrderId, Order[Order.State]] = new NotImplementedMap,
  idToWorkflow: PartialFunction[WorkflowId, Workflow] = new NotImplementedMap,
  keyToUnsignedItemState_ : Map[UnsignedItemKey, UnsignedItemState] = Map.empty)
extends EventDrivenStateView[TestStateView, Event]:
  val companion: TestStateView.type = TestStateView

  def applyEvent(keyedEvent: KeyedEvent[Event]) =
    keyedEvent match
      case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        applyOrderEvent(orderId, event)

      case _ => eventNotApplicable(keyedEvent)

  def orders = idToOrder.values

  def workflowPathToId(workflowPath: WorkflowPath) =
    Left(Problem.pure("workflowPathToId is not implemented"))

  def keyToUnsignedItemState = keyToUnsignedItemState_.view

  final lazy val keyToItem: MapView[InventoryItemKey, InventoryItem] =
    new MapView[InventoryItemKey, InventoryItem]:
      def get(itemKey: InventoryItemKey): Option[InventoryItem] =
        itemKey match
          case WorkflowId.as(id) => idToWorkflow.get(id)
          case path: LockPath => keyToUnsignedItemState.get(path).map(_.item)
          case path: BoardPath => keyToUnsignedItemState.get(path).map(_.item)
          case path: CalendarPath => keyToUnsignedItemState.get(path).map(_.item)

      def iterator: Iterator[(InventoryItemKey, InventoryItem)] =
        throw new NotImplementedError

  override protected def update(
    orders: Iterable[Order[Order.State]],
    removeOrders: Iterable[OrderId],
    addItemStates: Iterable[UnsignedSimpleItemState],
    removeItemStates: Iterable[UnsignedSimpleItemPath])
  : Checked[TestStateView] =
    // Do not touch unused entries, they may be a NotImplementedMap
    var x = this
    if removeOrders.nonEmpty then x = x.copy(idToOrder = idToOrder -- removeOrders)
    if orders.nonEmpty then x = x.copy(idToOrder = idToOrder ++ orders.map(o => o.id -> o))
    if removeItemStates.nonEmpty then x = x.copy(keyToUnsignedItemState_ = keyToUnsignedItemState_ -- removeItemStates)
    if addItemStates.nonEmpty then x = x.copy(
      keyToUnsignedItemState_ = keyToUnsignedItemState_ ++ addItemStates.map(o => o.path -> o))
    Right(x)


object TestStateView extends EventDrivenState.Companion[TestStateView, Event]:
  def of(
    isAgent: Boolean,
    controllerId: ControllerId = ControllerId("CONTROLLER"),
    orders: Option[Iterable[Order[Order.State]]] = None,
    workflows: Option[Iterable[Workflow]] = None,
    itemStates: Iterable[UnsignedSimpleItemState] = Nil)
  = new TestStateView(
    isAgent, controllerId,
    idToOrder = orders.fold_(new NotImplementedMap, _.toKeyedMap(_.id)),
    idToWorkflow = workflows.fold_(new NotImplementedMap, _.toKeyedMap(_.id)),
    keyToUnsignedItemState_ = itemStates.toKeyedMap(_.path))
