package js7.data.execution.workflow.instructions

import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.execution.workflow.instructions.RetryExecutor._
import js7.data.order.OrderEvent.{OrderFailedIntermediate_, OrderRetrying}
import js7.data.order.OrderObstacle.WaitingForOtherTime
import js7.data.order.{Order, OrderObstacleCalculator}
import js7.data.state.StateView
import js7.data.workflow.instructions.{Retry, TryInstruction}
import js7.data.workflow.position.{BranchPath, TryBranchId}
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
private[instructions] final class RetryExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = Retry
  val instructionClass = classOf[Retry]

  def toEvents(retry: Retry, order: Order[Order.State], state: StateView) =
    if (!order.isState[Order.Ready])
      Right(Nil)
    else
      order.workflowPosition.position.nextRetryBranchPath
        .flatMap(branchPath =>
          branchPath.parent
            .flatMap(parent =>
              Some(state.instruction(order.workflowId /: parent))
                .collect { case o: TryInstruction => o }
                .flatMap(_try =>
                  branchPath.lastOption.map(_.branchId).collect {
                    case TryBranchId(index) => (_try.maxTries, _try.retryDelay(index))
                  }))
            .toChecked(missingTryProblem(branchPath))
            .map {
              case (Some(maxRetries), _) if order.position.tryCount >= maxRetries =>
                (order.id <-: OrderFailedIntermediate_()) :: Nil
              case (_, delay) =>
                (order.id <-: OrderRetrying(
                  movedTo = branchPath % 0,
                  delayedUntil = (delay.isPositive) ? nextTimestamp(delay))
                ):: Nil
              })

  private def nextTimestamp(delay: FiniteDuration) =
    clock.now() + delay match {
      case at if delay >= 10.s => at.roundToNextSecond
      case at => at
    }

  override def toObstacles(order: Order[Order.State], calculator: OrderObstacleCalculator) =
    order.state match {
      case Order.DelayedAfterError(until) =>
        Right(Set(WaitingForOtherTime(until)))

      case _ =>
        super.toObstacles(order, calculator)
    }
}

object RetryExecutor
{
  private def missingTryProblem(branchPath: BranchPath) =
    Problem.pure(s"Retry: branchPath does not denotes a 'try' statement: $branchPath")
}
