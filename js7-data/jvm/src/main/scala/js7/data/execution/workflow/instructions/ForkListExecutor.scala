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

object ForkListExecutor extends ForkInstructionExecutor[ForkList]:

  protected def toForkedEvent(
    instr: ForkList,
    order: Order[Order.IsFreshOrReady],
    engineState: EngineState,
    now: Timestamp)
  : Checked[OrderForked] =
      for
        scope0 <- engineState.toImpureOrderExecutingScope(order, now)
        scope = scope0 |+| AgentsSubagentIdsScope(engineState)
        elements <- instr.children.evalAsVector(using scope)
        childIds <- elements.traverseWithIndexM: (element, i) =>
          instr.childToId.eval(View(element, NumberValue(i)))(using scope)
            .flatMap(_.toStringValueString)
        _ <- childIds.checkUniqueness
          .left.map(Problem(s"Duplicate child IDs in ${instr.children}: ") |+| _)
        argsOfChildren <- elements.traverseWithIndexM: (element, i) =>
          instr.childToArguments.eval(View(element, NumberValue(i)))(using scope)
            .flatMap(_.as[ObjectValue])
        children <- childIds.zip(argsOfChildren).traverse: (childId, args) =>
          order.id.withChild(childId)
            .map(OrderForked.Child(_, args.nameToValue))
      yield
        OrderForked(children)

  protected def forkResult(
    fork: ForkList,
    order: Order[Order.Forked],
    state: EngineState,
    now: Timestamp) =
    OrderOutcome.Completed.fromChecked(
      for
        results <- order.state.children
          .map(_.orderId)
          .traverse: childOrderId =>
            calcResult(fork.workflow.result getOrElse Map.empty, childOrderId, state, now)
      yield
        OrderOutcome.Succeeded(results.view
          .flatten
          .groupMap(_._1)(_._2).view
          .mapValues(values => ListValue(values.toVector)).toMap))
