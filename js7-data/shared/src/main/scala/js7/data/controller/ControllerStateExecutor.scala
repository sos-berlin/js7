package js7.data.controller

import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.controller.ControllerStateExecutor._
import js7.data.event.{Event, KeyedEvent}
import js7.data.execution.workflow.{OrderEventHandler, OrderEventSource}
import js7.data.item.VersionId
import js7.data.order.OrderEvent.{OrderBroken, OrderCoreEvent, OrderForked, OrderLockEvent, OrderRemoveMarked}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder
import scala.collection.{View, mutable}

final class ControllerStateExecutor(private var _controllerState: ControllerState)
{
  private val orderEventSource = liveOrderEventSource(() => _controllerState)

  def controllerState = _controllerState

  private def workflowPathToVersionId(workflowPath: WorkflowPath): Option[VersionId] =
    _controllerState.repo.pathTo[Workflow](workflowPath).toOption.map(_.id.versionId)

  def applyEventsAndReturnSubsequentEvents(keyedEvents: Seq[KeyedEvent[Event]])
  : Seq[KeyedEvent[OrderCoreEvent]] = {
    _controllerState.applyEvents(keyedEvents) match {
      case Left(problem) => scribe.error(problem.toString)  // ???
      case Right(o) => _controllerState = o
    }
    val eventOrderIds = keyedEvents.view.map(_.key).collect { case o: OrderId => o }.toSeq.distinct
    nextOrderEventsByOrderId(eventOrderIds) ++
      nextOrderEvents
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

  def nextOrderEventsByOrderId(orderIds: Seq[OrderId]): Seq[KeyedEvent[OrderCoreEvent]] = {
    val queue = mutable.Queue.empty[OrderId] ++= orderIds
    val _keyedEvents = new VectorBuilder[KeyedEvent[OrderCoreEvent]]
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
    _keyedEvents.result()
  }

  private def keyedEventToOrderIds(keyedEvent: KeyedEvent[OrderEvent]): View[OrderId] =
    View(keyedEvent.key) ++ (keyedEvent.event match {
      case OrderLockEvent(lockIds) =>
        lockIds.view
          .flatMap(_controllerState.idToLockState.get)
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
      id => controllerState().idToLockState.checked(id),
      isAgent = false)

  def liveOrderEventHandler(controllerState: () => ControllerState) =
    new OrderEventHandler(
      id => controllerState().repo.idTo[Workflow](id),
      id => controllerState().idToOrder.checked(id))
}
