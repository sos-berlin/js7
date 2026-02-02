package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.data.event.{EventCalc, KeyedEvent}
import js7.data.execution.workflow.OrderEventSource.moveOrderToNextInstruction
import js7.data.execution.workflow.instructions.SleepExecutor.*
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderSleeping}
import js7.data.order.OrderObstacle.WaitingForOtherTime
import js7.data.order.{Order, OrderId, OrderObstacle, OrderObstacleCalculator}
import js7.data.state.EngineState_
import js7.data.value.NumberValue
import js7.data.workflow.instructions.Sleep
import scala.concurrent.duration.*

private object SleepExecutor extends EventInstructionExecutor_[Sleep]:

  def toEventCalc[S <: EngineState_[S]](instr: Sleep, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    useOrder(orderId): (coll, order) =>
      order.ifState[Order.IsFreshOrReady].map: order =>
        start(coll, orderId): (coll, order) =>
          for
            scope <- coll.aggregate.toImpureOrderExecutingScope(order, coll.now)
            value <- instr.duration.eval(using scope).map(_.missingTo(NumberValue.Zero))
            numberValue <- value.toNumberValue
            duration = bigDecimalSecondsToDuration(numberValue.number)
            coll <-
              if duration.isPositive then
                coll:
                  order.id <-: OrderSleeping(coll.now + duration)
              else
                coll:
                  moveOrderToNextInstruction(order)
          yield coll
      .orElse:
        order.ifState[Order.Sleeping].map: order =>
          if order.state.until <= coll.now then
            coll:
              moveOrderToNextInstruction(order)
          else
            coll.nix
      .getOrElse:
        coll.nix

  override def toObstacles(
    order: Order[Order.State],
    calculator: OrderObstacleCalculator,
    now: Timestamp)
  : Checked[Set[OrderObstacle]] =
    order.state match
      case Order.Sleeping(until) =>
        Right(Set(WaitingForOtherTime(until)))

      case _ =>
        super.toObstacles(order, calculator, now)

  def bigDecimalSecondsToDuration(number: BigDecimal): FiniteDuration =
    val nanos = number * 1_000_000_000
    if nanos > Long.MaxValue then
      (100 * 365).days
    else if nanos < 0 then
      ZeroDuration
    else
      nanos.toLong.ns
