package js7.data.execution.workflow.instructions

import cats.instances.either.*
import cats.instances.vector.*
import cats.syntax.traverse.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits.*
import js7.data.order.OrderEvent.OrderForked
import js7.data.order.{Order, OrderOutcome}
import js7.data.state.{AgentsSubagentIdsScope, EngineState}
import js7.data.value.{ListValue, NumberValue, ObjectValue}
import js7.data.workflow.instructions.ForkList
import scala.collection.View

private[instructions] final class ForkListExecutor(protected val service: InstructionExecutorService)
extends ForkInstructionExecutor:

  type Instr = ForkList
  val instructionClass = classOf[ForkList]

  protected def toForkedEvent(fork: ForkList, order: Order[Order.IsFreshOrReady], state: EngineState)
  : Checked[OrderForked] =
    for
      scope0 <- state.toImpureOrderExecutingScope(order, clock.now())
      scope =  scope0 |+| new AgentsSubagentIdsScope(state)
      elements <- fork.children.evalAsVector(using scope)
      childIds <- elements
        .traverseWithIndexM { case (element, i) =>
          fork.childToId
            .eval(View(element, NumberValue(i)))(using scope)
            .flatMap(_.toStringValueString)
        }
      _ <- childIds.checkUniqueness
        .left.map(Problem(s"Duplicate child IDs in ${fork.children}: ") |+| _)
      argsOfChildren <- elements
        .traverseWithIndexM { case (element, i) =>
          fork.childToArguments
            .eval(View(element, NumberValue(i)))(using scope)
            .flatMap(_.as[ObjectValue])
        }
      children <- childIds.zip(argsOfChildren)
        .traverse { case (childId, args) =>
          order.id.withChild(childId)
            .map(OrderForked.Child(_, args.nameToValue))
        }
    yield
      OrderForked(children)

  protected def forkResult(fork: ForkList, order: Order[Order.Forked], state: EngineState,
    now: Timestamp) =
    OrderOutcome.Completed.fromChecked(
      for
        results <- order.state.children
          .map(_.orderId)
          .traverse(childOrderId =>
            calcResult(fork.workflow.result getOrElse Map.empty, childOrderId, state, now))
      yield
        OrderOutcome.Succeeded(results.view
          .flatten
          .groupMap(_._1)(_._2).view
          .mapValues(values => ListValue(values.toVector)).toMap))
