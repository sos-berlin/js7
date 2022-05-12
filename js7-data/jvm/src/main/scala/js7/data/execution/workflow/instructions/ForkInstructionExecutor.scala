package js7.data.execution.workflow.instructions

import cats.instances.either._
import cats.instances.vector._
import cats.syntax.traverse._
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.typeclasses.IsEmpty.syntax.toIsEmptyAllOps
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.instructions.ForkInstructionExecutor._
import js7.data.order.Order.Cancelled
import js7.data.order.OrderEvent.{OrderActorEvent, OrderAttachable, OrderDetachable, OrderFailedIntermediate_, OrderForked, OrderJoined}
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.state.StateView
import js7.data.value.Value
import js7.data.value.expression.Expression
import js7.data.workflow.instructions.ForkInstruction
import scala.collection.mutable

trait ForkInstructionExecutor extends EventInstructionExecutor
{
  type Instr <: ForkInstruction

  protected val service: InstructionExecutorService
  private implicit val implicitService = service

  private[execution] final def tryJoinChildOrder(
    fork: Instr,
    childOrder: Order[Order.State],
    state: StateView)
  =
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
              toJoined(parentOrder, fork, state)
            })

  protected[instructions] final def toJoined(
    order: Order[Order.Forked],
    fork: Instr,
    state: StateView)
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
          val now = clock.now()
          order.id <-: OrderJoined(
            if (!allSucceeded)
              Outcome.Failed(toJoinFailedMessage(order, state))
            else
              forkResult(fork, order, state, now))
        }
      }

  protected def forkResult(fork: Instr, order: Order[Order.Forked], state: StateView,
    now: Timestamp): Outcome.Completed

  protected def calcResult(resultExpr: Map[String, Expression], childOrderId: OrderId,
    state: StateView, now: Timestamp)
  : Checked[Vector[(String, Value)]] =
    state.idToOrder.checked(childOrderId)
      .flatMap(childOrder =>
        resultExpr
          .toVector
          .traverse { case (name, expr) =>
            for {
              scope <- state.toImpureOrderExecutingScope(childOrder, now)
              value <- expr.eval(scope)
            } yield name -> value
          })

  private def toJoinFailedMessage(order: Order[Order.Forked], state: StateView): Option[String] = {
    order.state.children.view
      .flatMap { child =>
        val childOrder = state.idToOrder(child.orderId)
        if (childOrder.isState[Cancelled])
          Some(s"${child.orderId} has been cancelled")
        else
          state.idToOrder(child.orderId)
            .lastOutcome
            .match_ {
              case Outcome.Failed(maybeErr, _) =>
                Some(child.orderId.toString + " failed" + (maybeErr.fold("")(": " + _)))
              case _ => None
            }
      }
      .take(3)  // Avoid huge error message
      .emptyToNone
      .map(_.mkString(";\n"))
  }

  private def withCacheAccess[A](order: Order[Order.Forked], state: StateView)
    (body: service.forkCache.Access => A)
  : A =
    service.forkCache.synchronized {
      val entry = service.forkCache.ensureEntry(
        order.id,
        order.state.children
          .view
          .map(o => state.idToOrder(o.orderId))
          .filter(state.childOrderEnded(_, parent = order))
          .map(_.id))
      body(entry)
    }

  protected[instructions] final def postprocessOrderForked(
    fork: ForkInstruction,
    order: Order[Order.Ready],
    orderForked: OrderForked,
    state: StateView)
  : Checked[OrderActorEvent] =
    for (_ <- checkOrderIdCollisions(orderForked, state)) yield
      predictAttachOrDetach(fork, order, orderForked, state)
        .getOrElse(orderForked)

  // Forking many children at Controller and then attached all to their first agent is inefficient.
  // Here we decide where to attach the forking order before generating child orders.
  protected[instructions] final def predictAttachOrDetach(
    fork: ForkInstruction,
    order: Order[Order.Ready],
    orderForked: OrderForked,
    state: StateView)
  : Option[OrderActorEvent] = {
    val eventSource = new OrderEventSource(state)
    order
      .newForkedOrders(orderForked)
      .toVector
      .traverse(eventSource.nextAgent)
      .map(_.toSet)
    match {
      case Left(problem) =>
        Some(OrderFailedIntermediate_(Some(Outcome.Failed.fromProblem(problem))))

      case Right(agentPaths: Set[Option[AgentPath]]) =>
        // None means Controller or the location is irrelevant (not distinguishable for now)
        if (orderForked.children.sizeIs == 0)
          None  // no children
        else if (agentPaths.sizeIs == 1) {
          // All children start at the same agent
          val enoughChildren = orderForked.children.sizeIs >= minimumChildCountForParentAttachment
          (order.attachedState, agentPaths.head) match {
            case (Some(Order.Attached(parentsAgentPath)), Some(childrensAgentPath)) =>
              // If parent order's attachment and the orders first agent differs,
              // detach the parent order!
              (parentsAgentPath != childrensAgentPath) ?
                OrderDetachable

            case (Some(Order.Attached(parentsAgentPath)), None) =>
              // If the orders first location is the Controller or irrelevant,
              // then we prefer to detach the parent order.
              (fork.agentPath.exists(_ != parentsAgentPath) || enoughChildren) ?
                OrderDetachable

            case (None, Some(childrensAgentPath)) =>
              // Attach the forking order to the children's agent
              (fork.agentPath.contains(childrensAgentPath) || enoughChildren) ?
                OrderAttachable(childrensAgentPath)

            case _ =>
              None
          }
        } else {
          // Child orders may start at different agents
          order.attachedState match {
            case Some(Order.Attached(parentsAgentPath)) =>
              // We prefer to detach
              // Inefficient if only one of many children change the agent !!!
              !fork.agentPath.contains(parentsAgentPath) ?
                OrderDetachable

            case _ =>
              fork.agentPath.map(OrderAttachable(_))
          }
        }
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
