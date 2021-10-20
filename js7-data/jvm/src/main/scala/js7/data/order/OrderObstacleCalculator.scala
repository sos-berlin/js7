package js7.data.order

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.order.OrderObstacle.{WaitingForCommand, WaitingForTime}
import js7.data.state.StateView

object OrderObstacleCalculator
{
  private val noObstacles: Checked[Set[OrderObstacle]] =
    Right(Set.empty)

  def orderToObstacles(orderId: OrderId, state: StateView)
    (implicit instructionExecutorService: InstructionExecutorService)
  : Checked[Set[OrderObstacle]] =
    for {
      order <- state.idToOrder.checked(orderId)
      a <- instructionExecutorService.toObstacles(order, state)
      b <- orderStateToObstacles(order)
    } yield a ++ b

  private def orderStateToObstacles(order: Order[Order.State])
  : Checked[Set[OrderObstacle]] =
    order.state match {
      case Order.Fresh =>
        Right(order.scheduledFor
          .map(WaitingForTime(_))
          .toSet)

      case Order.FailedWhileFresh | Order.Failed | Order.Cancelled =>
        Right(Set(WaitingForCommand))

      case _ =>
        noObstacles
    }
}
