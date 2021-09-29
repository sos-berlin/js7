package js7.data.execution.workflow.instructions

import cats.instances.either._
import cats.syntax.semigroup._
import cats.syntax.traverse._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits.RichIterable
import js7.data.event.KeyedEvent
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderActorEvent, OrderFailedIntermediate_, OrderForked, OrderMoved}
import js7.data.state.StateView
import js7.data.value.NumberValue
import js7.data.workflow.instructions.ForkList
import scala.collection.View

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
            joined <- toJoined(order, fork, state)
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
      scope <- state.toImpureOrderExecutingScope(order, clock.now())
      elements <- fork.children.evalAsVector(scope)
      childIds <- elements
        .traverseWithIndexM { case (element, i) =>
          fork.childToId
            .eval(View(element, NumberValue(i)))(scope)
            .flatMap(_.toStringValueString)
        }
      _ <- childIds.checkUniqueness
        .mapProblem(Problem(s"Duplicate child IDs in ${fork.children}: ") |+| _)
      argsOfChildren <- elements
        .traverseWithIndexM { case (element, i) =>
          fork.childToArguments
            .eval(View(element, NumberValue(i)))(scope)
            .flatMap(_.asObjectValue)
        }
      children <- childIds.zip(argsOfChildren)
        .traverse { case (childId, args) =>
          order.id.withChild(childId)
            .map(OrderForked.Child(_, args.nameToValue))
        }
      orderForked = OrderForked(children)
      event <- postprocessOrderForked(fork, order, orderForked, state)
    } yield (order.id <-: event) :: Nil
}
