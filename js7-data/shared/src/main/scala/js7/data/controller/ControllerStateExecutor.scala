package js7.data.controller

import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichEither, RichPartialFunction}
import js7.data.Problems.AgentResetProblem
import js7.data.agent.AgentPath
import js7.data.agent.AgentRefStateEvent.AgentResetStarted
import js7.data.controller.ControllerStateExecutor._
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{Event, KeyedEvent}
import js7.data.execution.workflow.{OrderEventHandler, OrderEventSource}
import js7.data.item.BasicItemEvent.{ItemAttachable, ItemDestroyed, ItemDetachable, ItemDetached}
import js7.data.item.ItemAttachedState.{Attachable, Attached, Detachable}
import js7.data.item.{BasicItemEvent, InventoryItemEvent, InventoryItemKey, VersionId}
import js7.data.order.Order.State
import js7.data.order.OrderEvent.{OrderBroken, OrderCoreEvent, OrderDetached, OrderForked, OrderLockEvent, OrderProcessed, OrderRemoveMarked}
import js7.data.order.{Order, OrderEvent, OrderId, Outcome}
import js7.data.workflow.{Workflow, WorkflowPath}
import scala.annotation.tailrec
import scala.collection.{View, mutable}

final class ControllerStateExecutor(private var _controllerState: ControllerState)
{
  private val orderEventSource = toLiveOrderEventSource(() => _controllerState)

  def controllerState = _controllerState

  private def workflowPathToVersionId(workflowPath: WorkflowPath): Option[VersionId] =
    _controllerState.repo.pathTo[Workflow](workflowPath).toOption.map(_.id.versionId)

  def resetAgent(agentPath: AgentPath): Seq[KeyedEvent[Event]] = {
    val agentResetStarted = View(agentPath <-: AgentResetStarted)
    val ordersDetached = controllerState.idToOrder.values.view
      .flatMap(resetAgentForOrder(_, agentPath))
    val itemsDetached = controllerState.itemToAgentToAttachedState.to(View)
      .filter(_._2.contains(agentPath))
      .map(_._1)
      .map(itemKey => NoKey <-: ItemDetached(itemKey, agentPath))
    (agentResetStarted ++ ordersDetached ++ itemsDetached).toVector
  }

  private def resetAgentForOrder(order: Order[Order.State], agentPath: AgentPath)
  : Seq[KeyedEvent[OrderCoreEvent]] = {
    val outcome = Outcome.Disrupted(AgentResetProblem(agentPath))
    order.attachedState match {
      case Some(Order.AttachedState.HasAgentPath(`agentPath`)) =>
        val stateEvents = (order.state: State) match {
          case Order.Processing =>
            Vector(order.id <-: OrderProcessed(outcome))
          //case _: Order.Offering =>
          //case _: Order.Awaiting =>
          case _ => Vector.empty
        }
        val detached = stateEvents :+ (order.id <-: OrderDetached)
        val detachedState = _controllerState.applyEvents(detached).orThrow
        val fail = toLiveOrderEventSource(() => detachedState)
          .failOrDetach(detachedState.idToOrder(order.id), Some(outcome), uncatchable = true)
          .orThrow
        detached :+ (order.id <-: fail)

      case _ => Nil
    }
  }

  def applyEventsAndReturnSubsequentEvents(keyedEvents: Seq[KeyedEvent[Event]])
  : Seq[KeyedEvent[Event]] = {
    _controllerState.applyEvents(keyedEvents) match {
      case Left(problem) => scribe.error(problem.toString)  // ???
      case Right(o) => _controllerState = o
    }
    val touchedItemKeys = keyedEvents
      .collect { case KeyedEvent(_, e: InventoryItemEvent) => e.key }
      .distinct
    val touchedOrderIds = keyedEvents
      .collect { case KeyedEvent(k: OrderId, _) => k }
      .distinct
    (nextItemEvents(touchedItemKeys).view ++
      nextOrderEventsByOrderId(touchedOrderIds) ++
      nextOrderEvents
    ).toVector
  }

  private def nextItemEvents(itemKeys: Seq[InventoryItemKey]): Seq[KeyedEvent[BasicItemEvent]] =
    itemKeys
      .flatMap(itemKey => controllerState.keyToItem.get(itemKey).view)
      .flatMap(item =>
        _controllerState.itemToAgentToAttachedState.get(item.key) match {
          case Some(agentPathToAttached) =>
            agentPathToAttached.view.flatMap {
              case (agentPath, Attached(revision)) =>
                if (controllerState.deleteItems(item.key))
                  Some(ItemDetachable(item.key, agentPath))
                else
                  (item.itemRevision != revision) ?
                    (item.dedicatedAgentPath match {
                      case Some(`agentPath`) | None =>
                        // Item is dedicated to this Agent or is required by an Order.
                        // Attach again without detaching, and let Agent change the item while in flight
                        ItemAttachable(item.key, agentPath)
                      case Some(_) =>
                        // Item's Agent dedication has changed, so we detach it
                        ItemDetachable(item.key, agentPath)
                    })

              case (_, Attachable | Detachable) =>
                None
            }

          case None =>
            if (controllerState.deleteItems(item.key))
              controllerState.isItemDestroyable(item.key).thenList(ItemDestroyed(item.key))
            else
              item.dedicatedAgentPath.map(ItemAttachable(item.key, _))
        })
        .map(NoKey <-: _)

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
  def toLiveOrderEventSource(controllerState: () => ControllerState) =
    new OrderEventSource(
      id => controllerState().idToOrder.checked(id),
      id => controllerState().repo.idTo[Workflow](id),
      id => controllerState().pathToLockState.checked(id),
      isAgent = false)

  def toLiveOrderEventHandler(controllerState: () => ControllerState) =
    new OrderEventHandler(
      id => controllerState().repo.idTo[Workflow](id),
      id => controllerState().idToOrder.checked(id))
}
