package js7.data.order

import cats.syntax.traverse.*
import java.util.concurrent.ConcurrentHashMap
import js7.base.problem.Checked
import js7.base.time.{Timestamp, WallClock}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentRef
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.job.JobKey
import js7.data.order.Order.Processing
import js7.data.order.OrderObstacle.{AgentProcessLimitReached, WaitingForAdmission, WaitingForCommand, WaitingForOtherTime}
import js7.data.state.StateView
import scala.collection.{View, mutable}

final class OrderObstacleCalculator(val stateView: StateView):
  // TODO Slow !!!
  def waitingForAdmissionOrderCount(now: Timestamp): Int =
    ordersToObstacles(stateView.orders.view.map(_.id), now)
      .orThrow // no exception expected
      .count(_._2.exists {
        case _: WaitingForAdmission => true
        case _ => false
      })

  def ordersToObstacles(orderIds: Iterable[OrderId], now: Timestamp)
  : Checked[View[(OrderId, Set[OrderObstacle])]] =
    val instructionService = new InstructionExecutorService(WallClock.fixed(now))
    orderIds
      .toVector
      .traverse(orderId =>
        orderToObstacles(orderId)(instructionService)
          .map(obstacles => orderId -> obstacles))
      .map(_
        .view
        .filter(_._2.nonEmpty))

  def orderToObstacles(orderId: OrderId)
    (implicit instructionExecutorService: InstructionExecutorService)
  : Checked[Set[OrderObstacle]] =
    for
      order <- stateView.idToOrder.checked(orderId)
      a <- instructionExecutorService.toObstacles(order, this)
      b = orderStateToObstacles(order)
      c = order.isSuspendedOrStopped.thenSet[OrderObstacle](WaitingForCommand)

    yield a ++ b ++ c ++ workflowSuspendedObstacle(order)

  private def workflowSuspendedObstacle(order: Order[Order.State]) =
    stateView.isWorkflowSuspended(order.workflowPath) ? OrderObstacle.WorkflowSuspended

  private def orderStateToObstacles(order: Order[Order.State]): Set[OrderObstacle] =
    val result = mutable.Set.empty[OrderObstacle]
    order.state match
      case Order.Fresh =>
        result ++= order.scheduledFor.map(WaitingForOtherTime(_))

      case Order.FailedWhileFresh | Order.Failed | Order.Cancelled =>
        result += WaitingForCommand

      case _ =>

    if order.isState[Order.IsFreshOrReady] then
      for agentPath <- order.attached do
        val limit = stateView.keyToItem(AgentRef).get(agentPath).flatMap(_.processLimit)
        if limit.exists(_ <= stateView.slowProcessingOrderCount(agentPath)) then
          result += AgentProcessLimitReached
    result.toSet

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
