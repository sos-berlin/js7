package js7.data.execution.workflow.instructions

import cats.instances.either._
import cats.syntax.semigroup._
import cats.syntax.traverse._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.OrderEventSource
import js7.data.execution.workflow.context.StateView
import js7.data.execution.workflow.instructions.ForkInstructionExecutor.checkOrderIdCollisions
import js7.data.order.OrderEvent.{OrderActorEvent, OrderAttachable, OrderDetachable, OrderFailedIntermediate_, OrderForked, OrderMoved}
import js7.data.order.{Order, OrderId, Outcome}
import js7.data.value.{ListValue, StringValue}
import js7.data.workflow.instructions.ForkList

private[instructions] final class ForkListExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor with ForkInstructionExecutor
{
  type Instr = ForkList

  def toEvents(fork: ForkList, order: Order[Order.State], state: StateView) =
    start(order)
      .getOrElse(order
        .ifState[Order.Ready].map(
          toForkedEvent(fork, _, state))
        .orElse(
          for {
            order <- order.ifState[Order.Forked]
            joined <- toJoined(state, order)
          } yield Right(joined :: Nil))
        .orElse(order.ifState[Order.Processed].map { order =>
          val event = if (order.lastOutcome.isSucceeded)
            OrderMoved(order.position.increment)
          else
            OrderFailedIntermediate_()
          Right((order.id <-: event) :: Nil)
        })
        .getOrElse(Right(Nil)))

  private def toForkedEvent(fork: ForkList, order: Order[Order.Ready], state: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    for {
      scope <- state.toScope(order)
      values <- fork.children.evalAsVector(scope)
      childStrings <- values.traverse(_.toStringValueString)
      childIds <- childStrings.traverse(OrderId.ChildId.checked)
      _ <- childIds.checkUniqueness
        .mapProblem(Problem(s"Duplicate fork values in ${fork.children}: ") |+| _)
      children <- childIds
        .traverse(childId =>
          fork.childToArguments
            .eval(ListValue(Seq(StringValue(childId.string))))(scope)
            .flatMap(_.asObjectValue)
          .map(args => OrderForked.Child(order.id | childId, args.nameToValue)))
      orderForked = OrderForked(children)
      _ <- checkOrderIdCollisions(state, orderForked)
      event = predictAttachOrDetach(order, orderForked, state)
        .getOrElse(orderForked)
    } yield (order.id <-: event) :: Nil

  // Forking many children at Controller and then attached all to their first agent is inefficient.
  // Here we decide where to attach the forking order before generating child orders.
  private def predictAttachOrDetach(order: Order[Order.Ready], orderForked: OrderForked, state: StateView)
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
              // Three or more children? We attach the forking order.
              (agentPathSeq.sizeIs >= 3) ? OrderAttachable(agentPath)

            case _ =>
              None
          }
        else
          order.isAttached ? OrderDetachable
    }
  }
}
