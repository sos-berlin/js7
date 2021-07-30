package js7.data.execution.workflow.instructions

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.context.StateView
import js7.data.order.OrderEvent.{OrderActorEvent, OrderBroken, OrderDetachable, OrderFailedIntermediate_, OrderForked, OrderJoined}
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.workflow.instructions.ForkInstruction
import scala.collection.mutable

trait ForkInstructionExecutor extends EventInstructionExecutor
{
  private[workflow] def tryJoinChildOrder(
    state: StateView,
    childOrder: Order[Order.State],
    fork: ForkInstruction)
  : Option[KeyedEvent[OrderActorEvent]] =
    if (childOrder.isAttached)
      Some(childOrder.id <-: OrderDetachable)
    else {
      childOrder.parent
        .flatMap(state.idToOrder.get)
        .flatMap(_.ifState[Order.Forked])
        .flatMap { parentOrder =>
          service.forkCache.onChildBecameJoinable(childOrder.id, parentOrder.id)
          toJoined(state, parentOrder)
        }
    }

  private[workflow] def toJoined(state: StateView, order: Order[Order.Forked])
  : Option[KeyedEvent[OrderActorEvent]] =
    if (order.isAttached)
      Some(order.id <-: OrderDetachable)
    else {
      val joinableCount = service.forkCache.joinablesCount(order.id,
        default = order.state.children
          .view
          .map(o => state.idToOrder(o.orderId))
          .filter(state.childOrderEnded(_, parent = order))
          .map(_.id))

      (order.state.children.sizeIs == joinableCount) ? {
        val allSucceeded = order.state.children.view
          .map(child => state.idToOrder(child.orderId))
          .forall(_.lastOutcome.isSucceeded)
        service.forkCache.onJoined(order.id)
        order.id <-: OrderJoined(
          if (allSucceeded)
            Outcome.succeeded
          else
            Outcome.failed)
      }
    }
}

private[workflow] object ForkInstructionExecutor
{
  // Paranoid check. Will not work poperly on Agent because Agent does not know all orders.
  // The Order child syntax is based on the reserved character '|'.
  // It is not possible to generate a child order but with Fork.
  // So we can safely generate child orders because only Fork can do this.
  private[instructions] def checkOrderIdCollisions(
    state: StateView,
    orderForked: OrderForked)
  : Checked[Unit] = {
    val duplicates = orderForked.children.map(_.orderId).flatMap(state.idToOrder.get)
    if (duplicates.nonEmpty) {
      // Internal error, maybe a lost event OrderDetached
      val problem = Problem.pure(s"Forked OrderIds duplicate existing ${duplicates mkString ", "}")
      scribe.error(problem.toString)
      Left(problem)
    } else
      Checked.unit
  }

  private[instructions] class Cache
  {
    // Synchronization is not really needed, because it's used serially
    private val forkedToJoinableChildren = mutable.Map.empty[OrderId, mutable.Set[OrderId]]

    private[ForkInstructionExecutor] def onChildBecameJoinable(
      childOrderId: OrderId,
      parentOrderId: OrderId)
    : Unit =
      synchronized {
        for (joinables <- forkedToJoinableChildren.get(parentOrderId)) {
          joinables += childOrderId
        }
      }

    private[ForkInstructionExecutor] def joinablesCount(parentOrderId: OrderId, default: => Iterable[OrderId]): Int =
      synchronized {
        forkedToJoinableChildren.getOrElseUpdate(parentOrderId, default.to(mutable.Set))
          .size
      }

    private[ForkInstructionExecutor] def onJoined(parentOrderId: OrderId): Unit =
      synchronized {
        forkedToJoinableChildren -= parentOrderId
      }
  }
}
