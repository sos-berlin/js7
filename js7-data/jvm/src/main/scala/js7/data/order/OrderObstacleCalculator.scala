package js7.data.order

import java.util.concurrent.ConcurrentHashMap
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.job.JobKey
import js7.data.order.Order.Processing
import js7.data.order.OrderObstacle.{WaitingForCommand, WaitingForOtherTime}
import js7.data.order.OrderObstacleCalculator.noObstacles
import js7.data.state.StateView

final class OrderObstacleCalculator(val stateView: StateView)
{
  def orderToObstacles(orderId: OrderId)
    (implicit instructionExecutorService: InstructionExecutorService)
  : Checked[Set[OrderObstacle]] =
    for {
      order <- stateView.idToOrder.checked(orderId)
      a <- instructionExecutorService.toObstacles(order, this)
      b <- orderStateToObstacles(order)
    } yield a ++ b

  private def orderStateToObstacles(order: Order[Order.State])
  : Checked[Set[OrderObstacle]] =
    order.state match {
      case Order.Fresh =>
        Right(order.scheduledFor
          .map(WaitingForOtherTime(_))
          .toSet)

      case Order.FailedWhileFresh | Order.Failed | Order.Cancelled =>
        Right(Set(WaitingForCommand))

      case _ =>
        noObstacles
    }

  private val _jobToOrderCount = new ConcurrentHashMap[JobKey, Int]()

  def jobToOrderCount(jobKey: JobKey): Int =
    _jobToOrderCount.computeIfAbsent(
      jobKey,
      jobKey => stateView
        .idToWorkflow
        .get(jobKey.workflowId)
        .fold(0)(workflow =>
          stateView.orders.view
            .count(order =>
              order.state.isInstanceOf[Processing] &&
                order.workflowId == jobKey.workflowId &&
                workflow.positionToJobKey(order.position).contains(jobKey))))
}

object OrderObstacleCalculator
{
  private val noObstacles: Checked[Set[OrderObstacle]] =
    Right(Set.empty)
}
