package js7.data.state

import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.NotImplementedMap
import js7.base.utils.ScalaUtils.syntax._
import js7.data.board.{BoardPath, BoardState}
import js7.data.controller.ControllerId
import js7.data.event.{Event, EventDrivenState, KeyedEvent}
import js7.data.item.{InventoryItem, InventoryItemKey}
import js7.data.lock.{LockPath, LockState}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import scala.collection.MapView

case class TestStateView(
  isAgent: Boolean,
  controllerId: ControllerId = ControllerId("CONTROLLER"),
  idToOrder: Map[OrderId, Order[Order.State]] = new NotImplementedMap,
  idToWorkflow: PartialFunction[WorkflowId, Workflow] = new NotImplementedMap,
  pathToLockState: Map[LockPath, LockState] = new NotImplementedMap,
  pathToBoardState: Map[BoardPath, BoardState] = new NotImplementedMap)
extends EventDrivenStateView[TestStateView, Event]
{
  val companion = TestStateView

  def applyEvent(keyedEvent: KeyedEvent[Event]) =
    keyedEvent match {
      case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
        applyOrderEvent(orderId, event)

      case _ => eventNotApplicable(keyedEvent)
    }

  def orders = idToOrder.values

  def workflowPathToId(workflowPath: WorkflowPath) =
    Left(Problem.pure("workflowPathToId is not implemented"))

  lazy val keyToItem: MapView[InventoryItemKey, InventoryItem] =
    new MapView[InventoryItemKey, InventoryItem] {
      def get(itemKey: InventoryItemKey): Option[InventoryItem] =
        itemKey match {
          case WorkflowId.as(id) => idToWorkflow.get(id)
          case path: LockPath => pathToLockState.get(path).map(_.item)
          case path: BoardPath => pathToBoardState.get(path).map(_.item)
        }

      def iterator: Iterator[(InventoryItemKey, InventoryItem)] =
        throw new NotImplementedError
    }

  override protected def update(
    removeOrders: Iterable[OrderId],
    orders: Iterable[Order[Order.State]],
    lockStates: Iterable[LockState],
    boardStates: Iterable[BoardState])
  : Checked[TestStateView] = {
    // Do not touch unused entries, they may be a NotImplementedMap
    var x = this
    if (removeOrders.nonEmpty) x = x.copy(idToOrder = idToOrder -- removeOrders)
    if (orders.nonEmpty) x = x.copy(idToOrder = idToOrder ++ orders.map(o => o.id -> o))
    if (lockStates.nonEmpty) x = x.copy(pathToLockState = pathToLockState ++ lockStates.map(o => o.lock.path -> o))
    if (boardStates.nonEmpty) x = x.copy(pathToBoardState = pathToBoardState ++ boardStates.map(o => o.path -> o))
    Right(x)
  }
}

object TestStateView extends EventDrivenState.Companion[TestStateView, Event]
{
  def of(
    isAgent: Boolean,
    controllerId: ControllerId = ControllerId("CONTROLLER"),
    orders: Option[Iterable[Order[Order.State]]] = None,
    workflows: Option[Iterable[Workflow]] = None,
    lockStates: Option[Iterable[LockState]] = None,
    boardStates: Option[Iterable[BoardState]] = None)
  = new TestStateView(
    isAgent, controllerId,
    idToOrder = orders.fold_(new NotImplementedMap, _.toKeyedMap(_.id)),
    idToWorkflow = workflows.fold_(new NotImplementedMap, _.toKeyedMap(_.id)),
    pathToLockState = lockStates.fold_(new NotImplementedMap, _.toKeyedMap(_.lock.path)),
    pathToBoardState = boardStates.fold_(new NotImplementedMap, _.toKeyedMap(_.path)))
}
