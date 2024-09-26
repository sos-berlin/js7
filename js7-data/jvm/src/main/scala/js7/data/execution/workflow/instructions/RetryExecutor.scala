package js7.data.execution.workflow.instructions

import js7.base.problem.Checked.*
import js7.base.problem.Problem
import js7.base.time.ScalaTime.*
import js7.data.execution.workflow.instructions.RetryExecutor.*
import js7.data.order.OrderEvent.{OrderFailedIntermediate_, OrderMoved, OrderRetrying}
import js7.data.order.OrderObstacle.WaitingForOtherTime
import js7.data.order.{Order, OrderObstacleCalculator}
import js7.data.state.StateView
import js7.data.workflow.instructions.{Retry, TryInstruction}
import js7.data.workflow.position.*
import js7.data.workflow.position.BranchPath.syntax.*
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
private[instructions] final class RetryExecutor(protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = Retry
  val instructionClass = classOf[Retry]

  def toEvents(retry: Retry, order: Order[Order.State], state: StateView) =
    if !order.isState[Order.Ready] then
      Right(Nil)
    else
      order.workflowPosition.position.nextRetryPosition
        .flatMap: nextTryPos =>
          nextTryPos.branchPath.parent
            .flatMap: parent =>
              Some(state.instruction(order.workflowId /: parent))
                .collect:
                  case o: TryInstruction => o
                .flatMap: _try =>
                  nextTryPos.branchPath.lastOption.map(_.branchId).collect:
                    case TryBranchId(index) => (_try.maxTries, _try.retryDelay(index))

            .toChecked(missingTryProblem(nextTryPos))
            .map:
              case (Some(maxRetries), _) if order.position.tryCount >= maxRetries =>
                OrderFailedIntermediate_() :: Nil
              case (_, delay) =>
                if delay.isPositive then
                  OrderRetrying(Some(nextTimestamp(delay))) :: Nil
                else
                  OrderRetrying() :: OrderMoved(nextTryPos) :: Nil
            .map(_.map:
              order.id <-: _)

  private def nextTimestamp(delay: FiniteDuration) =
    clock.now() + delay match
      case at if delay >= 10.s => at.roundToNextSecond
      case at => at

  override def toObstacles(order: Order[Order.State], calculator: OrderObstacleCalculator) =
    order.state match
      case Order.DelayingRetry(until) =>
        Right(Set(WaitingForOtherTime(until)))

      case Order.DelayedAfterError(until) =>
        Right(Set(WaitingForOtherTime(until)))

      case _ =>
        super.toObstacles(order, calculator)


object RetryExecutor:
  private def missingTryProblem(position: Position) =
    Problem.pure(s"Retry: Order is not in a 'catch' block: $position")
