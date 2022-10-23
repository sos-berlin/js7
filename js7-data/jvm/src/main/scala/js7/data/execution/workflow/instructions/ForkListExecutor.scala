package js7.data.execution.workflow.instructions

import cats.instances.either.*
import cats.instances.vector.*
import cats.syntax.semigroup.*
import cats.syntax.traverse.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits.*
import js7.data.order.OrderEvent.{OrderActorEvent, OrderForked}
import js7.data.order.{Order, Outcome}
import js7.data.state.{AgentsSubagentIdsScope, StateView}
import js7.data.value.{ListValue, NumberValue}
import js7.data.workflow.instructions.ForkList
import scala.collection.View

private[instructions] final class ForkListExecutor(protected val service: InstructionExecutorService)
extends ForkInstructionExecutor
{
  type Instr = ForkList
  val instructionClass = classOf[ForkList]

  protected def toForkedEvent(fork: ForkList, order: Order[Order.Ready], state: StateView)
  : Checked[OrderActorEvent] =
    for {
      scope0 <- state.toImpureOrderExecutingScope(order, clock.now())
      scope =  scope0 |+| new AgentsSubagentIdsScope(state)
      elements <- fork.children.evalAsVector(scope)
      childIds <- elements
        .traverseWithIndexM { case (element, i) =>
          fork.childToId
            .eval(View(element, NumberValue(i)))(scope)
            .flatMap(_.toStringValueString)
        }
      _ <- childIds.checkUniqueness
        .left.map(Problem(s"Duplicate child IDs in ${fork.children}: ") |+| _)
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
    } yield event

  protected def forkResult(fork: ForkList, order: Order[Order.Forked], state: StateView,
    now: Timestamp) =
    Outcome.Completed.fromChecked(
      for {
        results <- order.state.children
          .map(_.orderId)
          .traverse(childOrderId =>
            calcResult(fork.workflow.result getOrElse Map.empty, childOrderId, state, now))
      } yield Outcome.Succeeded(results.view
        .flatten
        .groupMap(_._1)(_._2).view
        .mapValues(ListValue(_)).toMap))
}
