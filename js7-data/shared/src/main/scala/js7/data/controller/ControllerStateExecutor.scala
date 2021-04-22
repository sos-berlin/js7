package js7.data.controller

import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichPartialFunction}
import js7.data.controller.ControllerStateExecutor._
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, KeyedEvent}
import js7.data.execution.workflow.{OrderEventHandler, OrderEventSource}
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemDestroyed, ItemDetachable}
import js7.data.item.ItemAttachedState.Attached
import js7.data.item.{BasicItemEvent, InventoryItemEvent, InventoryItemKey, VersionId}
import js7.data.order.OrderEvent.{OrderBroken, OrderCoreEvent, OrderForked, OrderLockEvent, OrderRemoveMarked}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.orderwatch.OrderWatchPath
import js7.data.workflow.{Workflow, WorkflowPath}
import scala.annotation.tailrec
import scala.collection.{View, mutable}

final class ControllerStateExecutor(private var _controllerState: ControllerState)
{
  private val orderEventSource = liveOrderEventSource(() => _controllerState)

  def controllerState = _controllerState

  private def workflowPathToVersionId(workflowPath: WorkflowPath): Option[VersionId] =
    _controllerState.repo.pathTo[Workflow](workflowPath).toOption.map(_.id.versionId)

  def applyEventsAndReturnSubsequentEvents(keyedEvents: Seq[KeyedEvent[Event]])
  : Seq[KeyedEvent[Event]] = {
    _controllerState.applyEvents(keyedEvents) match {
      case Left(problem) => scribe.error(problem.toString)  // ???
      case Right(o) => _controllerState = o
    }
    val touchedItemIds = keyedEvents
      .collect { case KeyedEvent(_, e: InventoryItemEvent) => e.key }
      .distinct
    val touchedOrderIds = keyedEvents.view.map(_.key).collect { case o: OrderId => o }.toSeq.distinct
    (nextItemEvents(touchedItemIds).view ++
      nextOrderEventsByOrderId(touchedOrderIds) ++
      nextOrderEvents
    ).toVector
  }

  private def nextItemEvents(itemIds: Seq[InventoryItemKey]): Seq[KeyedEvent[BasicItemEvent]] =
    itemIds.flatMap {
      case itemId: OrderWatchPath =>
        controllerState.allOrderWatchesState.pathToOrderWatchState.get(itemId)
          .view.flatMap { orderWatchState =>
            import orderWatchState.orderWatch
            if (orderWatchState.agentPathToAttachedState.nonEmpty)
              orderWatchState.agentPathToAttachedState.flatMap {
                case (agentPath, Attached(revision)) =>
                  if (orderWatchState.delete)
                    Some(NoKey <-: ItemDetachable(itemId, agentPath))
                  else
                    (orderWatch.itemRevision != revision) ? (
                      if (agentPath == orderWatch.agentPath)
                        // Attach again without detaching, and let Agent change OrderWatch while in flight
                        NoKey <-: ItemAttachable(itemId, agentPath)
                      else
                        NoKey <-: ItemDetachable(itemId, agentPath))
                case _ =>
                  None
              }
            else if (orderWatchState.delete)
              orderWatchState.isDestroyable ? (NoKey <-: ItemDestroyed(itemId))
            else
              Some(NoKey <-: ItemAttachable(itemId, orderWatch.agentPath))
          }

      case _ =>
        None
    }

  def nextOrderEvents: View[KeyedEvent[OrderCoreEvent]] =
    _controllerState.allOrderWatchesState
      .nextEvents(workflowPathToVersionId)
      .filter {
        case KeyedEvent(orderId: OrderId, OrderRemoveMarked) =>
          // OrderWatchState emits OrderRemovedMarked without knowledge of the order
          controllerState.idToOrder.get(orderId).exists(o => !o.removeWhenTerminated)
        case _ => true
      }

  def nextOrderEventsByOrderId(orderIds: Seq[OrderId]): View[KeyedEvent[OrderCoreEvent]] = {
    val queue = mutable.Queue.empty[OrderId] ++= orderIds
    val _keyedEvents = mutable.Buffer.empty[KeyedEvent[OrderCoreEvent]]
    @tailrec def loop(): Unit = {
      queue.removeHeadOption() match {
        case Some(orderId) =>
          if (_controllerState.idToOrder contains orderId) {
            val keyedEvents = orderEventSource.nextEvents(orderId)
            for (KeyedEvent(orderId, OrderBroken(problem)) <- keyedEvents) {
              scribe.error(s"Order '${orderId.string}' is broken: $problem") // ???
            }
            _controllerState.applyEvents(keyedEvents) match {
              case Left(problem) => scribe.error(s"$orderId: $problem")  // ???
              case Right(o) =>
                _controllerState = o
                _keyedEvents ++= keyedEvents
                queue ++= keyedEvents.view.flatMap(keyedEventToOrderIds).toSeq.distinct
            }
          }
          loop()
        case None =>
      }
    }
    loop()
    _keyedEvents.view
  }

  private def keyedEventToOrderIds(keyedEvent: KeyedEvent[OrderEvent]): View[OrderId] =
    View(keyedEvent.key) ++ (keyedEvent.event match {
      case OrderLockEvent(lockPaths) =>
        lockPaths.view
          .flatMap(_controllerState.pathToLockState.get)
          .flatMap(_.firstQueuedOrderId)

      case OrderForked(children) =>
        children.view.map(_.orderId)

      case _ => View.empty
    })
}

object ControllerStateExecutor
{
  def liveOrderEventSource(controllerState: () => ControllerState) =
    new OrderEventSource(
      id => controllerState().idToOrder.checked(id),
      id => controllerState().repo.idTo[Workflow](id),
      id => controllerState().pathToLockState.checked(id),
      isAgent = false)

  def liveOrderEventHandler(controllerState: () => ControllerState) =
    new OrderEventHandler(
      id => controllerState().repo.idTo[Workflow](id),
      id => controllerState().idToOrder.checked(id))
}
