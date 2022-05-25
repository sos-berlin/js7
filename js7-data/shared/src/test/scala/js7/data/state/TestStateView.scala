package js7.data.state

import js7.base.problem.Problem
import js7.base.utils.NotImplementedMap
import js7.base.utils.ScalaUtils.syntax._
import js7.data.board.{BoardPath, BoardState}
import js7.data.controller.ControllerId
import js7.data.item.{InventoryItem, InventoryItemKey}
import js7.data.lock.{LockPath, LockState}
import js7.data.order.{Order, OrderId}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import scala.collection.MapView

case class TestStateView(
  isAgent: Boolean,
  controllerId: ControllerId = ControllerId("CONTROLLER"),
  idToOrder: Map[OrderId, Order[Order.State]] = new NotImplementedMap,
  idToWorkflow: PartialFunction[WorkflowId, Workflow] = new NotImplementedMap,
  pathToLockState: PartialFunction[LockPath, LockState] = new NotImplementedMap,
  pathToBoardState: PartialFunction[BoardPath, BoardState] = new NotImplementedMap)
extends StateView
{
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
}
