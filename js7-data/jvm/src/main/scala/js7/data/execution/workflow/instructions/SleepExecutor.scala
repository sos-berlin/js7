package js7.data.execution.workflow.instructions

import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.instructions.SleepExecutor.*
import js7.data.order.OrderEvent.{OrderMoved, OrderSleeping, OrderStarted}
import js7.data.order.OrderObstacle.WaitingForOtherTime
import js7.data.order.{Order, OrderObstacleCalculator}
import js7.data.state.EngineState
import js7.data.value.NumberValue
import js7.data.workflow.instructions.Sleep
import scala.concurrent.duration.*

private[instructions] final class SleepExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = Sleep
  val instructionClass = classOf[Sleep]

  def toEvents(instr: Sleep, order: Order[Order.State], state: EngineState)
  : Checked[List[KeyedEvent[OrderStarted | OrderSleeping | OrderMoved]]] =
    start(order).getOrElse:
      order.ifState[Order.Ready].map: order =>
        for
          scope <- state.toImpureOrderExecutingScope(order, clock.now())
          value <- instr.duration.eval(using scope).map(_.missingTo(NumberValue.Zero))
          number <- value.toNumberValue
          duration = bigDecimalSecondsToDuration(number.number)
        yield
          if duration.isPositive then
            (order.id <-: OrderSleeping(clock.now() + duration)) :: Nil
          else
            (order.id <-: OrderMoved(order.position.increment)) :: Nil
      .orElse:
        order.ifState[Order.Sleeping].map: order =>
          Right:
            order.state.until <= clock.now() thenList:
              order.id <-: OrderMoved(order.position.increment)
      .getOrElse:
        Right(Nil)

  override def toObstacles(order: Order[Order.State], calculator: OrderObstacleCalculator) =
    order.state match
      case Order.Sleeping(until) =>
        Right(Set(WaitingForOtherTime(until)))

      case _ =>
        super.toObstacles(order, calculator)


object SleepExecutor:

  private[instructions] def bigDecimalSecondsToDuration(number: BigDecimal): FiniteDuration =
    val nanos = number * 1_000_000_000
    if nanos > Long.MaxValue then
      (100 * 365).days
    else if nanos < 0 then
      ZeroDuration
    else
      nanos.toLong.ns
