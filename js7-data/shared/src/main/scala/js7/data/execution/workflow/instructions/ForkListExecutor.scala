package js7.data.execution.workflow.instructions

import cats.instances.either._
import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.context.StateView
import js7.data.execution.workflow.instructions.ForkInstructionExecutor.{checkOrderIdCollisions, toJoined}
import js7.data.order.OrderEvent.{OrderActorEvent, OrderFailedIntermediate_, OrderForked, OrderMoved}
import js7.data.order.{Order, OrderId}
import js7.data.value.{ListValue, StringValue}
import js7.data.workflow.instructions.ForkList

private[instructions] final class ForkListExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = ForkList

  def toEvents(fork: ForkList, order: Order[Order.State], state: StateView) =
    start(order)
      .getOrElse(order
        .ifState[Order.Ready].map(toForkedEvent(fork, _, state))
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
      values <- fork.children.evalAsList(scope)
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
      keyedEvent = checkOrderIdCollisions(state, order.id <-: OrderForked(children))
    } yield keyedEvent :: Nil
}
