package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.event.EventCalc
import js7.data.order.Order.Ready
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderPrompted}
import js7.data.order.OrderObstacle.WaitingForCommand
import js7.data.order.{Order, OrderId, OrderObstacle, OrderObstacleCalculator}
import js7.data.state.EngineState_
import js7.data.value.GoodValue
import js7.data.value.expression.Scope
import js7.data.workflow.instructions.Prompt

private object PromptExecutor extends EventInstructionExecutor_[Prompt]:

  def toEventCalc[S <: EngineState_[S]](instr: Prompt, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    detachOrder(orderId): (coll, order) =>
      start(coll, orderId): (coll, order) =>
        order.ifState[Ready].map: _ =>
          for
            given Scope <- coll.aggregate.toImpureOrderExecutingScope(order, coll.now)
            question <- instr.question.evalAs[GoodValue]
            coll <-
              coll:
                order.id <-: OrderPrompted(question)
          yield coll
        .getOrElse:
          coll.nix

  override def toObstacles(
    order: Order[Order.State],
    calculator: OrderObstacleCalculator,
    now: Timestamp)
  : Checked[Set[OrderObstacle]] =
    order.state match
      case Order.Prompting(_) =>
        Right(Set(WaitingForCommand))

      case _ =>
        super.toObstacles(order, calculator, now)
