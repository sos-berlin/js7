package js7.data.execution.workflow.instructions

import cats.syntax.traverse._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.context.StateView
import js7.data.execution.workflow.instructions.ForkInstructionExecutor._
import js7.data.order.OrderEvent.{OrderActorEvent, OrderAttachable, OrderDetachable, OrderFailedIntermediate_, OrderForked, OrderJoined}
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.workflow.instructions.ForkInstruction
import scala.collection.mutable

trait ForkInstructionExecutor extends EventInstructionExecutor
{
  private[workflow] final def tryJoinChildOrder(
    childOrder: Order[Order.State],
    evidence: ForkInstruction,
    state: StateView)
  : Option[KeyedEvent[OrderActorEvent]] =
    if (childOrder.isAttached)
      Some(childOrder.id <-: OrderDetachable)
    else
      childOrder.parent
        .flatMap(state.idToOrder.get)
        .flatMap(_.ifState[Order.Forked])
        .flatMap(parentOrder =>
          if (!parentOrder.isDetached)
            None
          else
            withCacheAccess(parentOrder, state) { cache =>
              cache.onChildBecameJoinable(childOrder.id)
              toJoined(parentOrder, state)
            })

  protected[instructions] final def toJoined(order: Order[Order.Forked], state: StateView)
  : Option[KeyedEvent[OrderActorEvent]] =
    if (order.isAttached)
      Some(order.id <-: OrderDetachable)
    else
      withCacheAccess(order, state) { cache =>
        (order.state.children.sizeIs == cache.numberOfJoinables) ? {
          cache.onJoined()
          val allSucceeded = order.state.children.view
            .map(child => state.idToOrder(child.orderId))
            .forall(_.lastOutcome.isSucceeded)
          order.id <-: OrderJoined(
            if (allSucceeded)
              Outcome.succeeded
            else
              Outcome.failed)
        }
      }

  private def withCacheAccess[A](order: Order[Order.Forked], state: StateView)
    (body: service.forkCache.Access => A)
  : A =
    service.forkCache.synchronized {
      val entry = service.forkCache.ensureEntry(order.id,
        init = order.state.children
        .view
        .map(o => state.idToOrder(o.orderId))
        .filter(state.childOrderEnded(_, parent = order))
        .map(_.id))
      body(entry)
    }

  protected[instructions] final def postprocessOrderForked(
    order: Order[Order.Ready],
    orderForked: OrderForked,
    state: StateView)
  : Checked[OrderActorEvent] =
    for (_ <- checkOrderIdCollisions(orderForked, state)) yield
      predictAttachOrDetach(order, orderForked, state)
        .getOrElse(orderForked)

  // Forking many children at Controller and then attached all to their first agent is inefficient.
  // Here we decide where to attach the forking order before generating child orders.
  protected[instructions] final def predictAttachOrDetach(
    order: Order[Order.Ready],
    orderForked: OrderForked,
    state: StateView)
  : Option[OrderActorEvent] = {
    val eventSource = new OrderEventSource(state)
    order
      .newForkedOrders(orderForked)
      .toVector
      .traverse(eventSource.nextAgent)
    match {
      case Left(problem) =>
        Some(OrderFailedIntermediate_(Some(Outcome.Failed.fromProblem(problem))))

      case Right(agentPathSeq: Vector[Option[AgentPath]]) =>
        // None means Controller (or unknown?)
        val agentPaths = agentPathSeq.toSet
        if (agentPaths.sizeIs == 0)
          None  // no children
        else if (agentPaths.sizeIs == 1)
          (order.attachedState, agentPaths.head) match {
            case (Some(Order.Attached(attached)), maybeAgentPath) =>
              !maybeAgentPath.contains(attached) ? OrderDetachable

            case (None, Some(agentPath)) =>
              // All children on the same agent? We attach the forking order.
              (agentPathSeq.sizeIs >= minimumChildCountForParentAttachment) ?
                OrderAttachable(agentPath)

            case _ =>
              None
          }
        else
          order.isAttached ? OrderDetachable
    }
  }
}

private object ForkInstructionExecutor
{
  private val minimumChildCountForParentAttachment = 3

  // Paranoid check. Will not work properly on Agent because Agent does not know all orders.
  // The Order child syntax is based on the reserved character '|'.
  // It is not possible to generate a child order but with Fork.
  // So we can safely generate child orders because only Fork can do this.
  private def checkOrderIdCollisions(orderForked: OrderForked, state: StateView): Checked[Unit] = {
    val duplicates = orderForked.children.map(_.orderId).flatMap(state.idToOrder.get)
    if (duplicates.nonEmpty) {
      // Internal error, maybe a lost OrderDetached event
      val problem = Problem.pure(s"Forked OrderIds duplicate existing ${duplicates mkString ", "}")
      scribe.error(problem.toString)
      Left(problem)
    } else
      Checked.unit
  }

  final class Cache
  {
    private val forkedToJoinableChildren = mutable.Map.empty[OrderId, mutable.Set[OrderId]]

    private[ForkInstructionExecutor] def ensureEntry(
      parentOrderId: OrderId,
      init: => Iterable[OrderId])
    : this.Access = {
      forkedToJoinableChildren.getOrElseUpdate(parentOrderId, init.to(mutable.Set))
      new Access(parentOrderId)
    }

    final class Access(parentOrderId: OrderId) {
      private[ForkInstructionExecutor] def onChildBecameJoinable(childOrderId: OrderId): Unit =
        forkedToJoinableChildren(parentOrderId) += childOrderId

      private[ForkInstructionExecutor] def numberOfJoinables: Int =
        forkedToJoinableChildren(parentOrderId).size

      private[ForkInstructionExecutor] def onJoined(): Unit =
        forkedToJoinableChildren -= parentOrderId
    }
  }
}
