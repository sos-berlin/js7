package js7.data.execution.workflow.instructions

import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.time.Timestamp
import js7.base.utils.ScalazStyle._
import js7.data.execution.workflow.context.OrderContext
import js7.data.execution.workflow.instructions.RetryExecutor._
import js7.data.order.OrderEvent.{OrderFailed, OrderRetrying}
import js7.data.order.{Order, Outcome}
import js7.data.workflow.instructions.{Retry, TryInstruction}
import js7.data.workflow.position.{BranchPath, TryBranchId}
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class RetryExecutor(clock: () => Timestamp) extends EventInstructionExecutor
{
  type Instr = Retry

  def toEvent(context: OrderContext, order: Order[Order.State], retry: Retry) =
    if (!order.isState[Order.Ready])
      Right(None)
    else
      order.workflowPosition.position.nextRetryBranchPath
        .flatMap(branchPath =>
          branchPath.parent
            .flatMap(parent =>
              Some(context.instruction(order.workflowId /: parent))
                .collect { case o: TryInstruction => o }
                .flatMap(_try =>
                  branchPath.lastOption.map(_.branchId).collect {
                    case TryBranchId(index) => (_try.maxTries, _try.retryDelay(index))
                  }))
            .toChecked(missingTryProblem(branchPath))
            .map {
              case (Some(maxRetries), _) if order.position.tryCount >= maxRetries =>
                Some(order.id <-: OrderFailed(Outcome.Disrupted(Problem.pure(s"Retry stopped because maxRetries=$maxRetries has been reached"))))
              case (_, delay) =>
                Some(order.id <-: OrderRetrying(
                  movedTo = branchPath % 0,
                  delayedUntil = (delay > Duration.Zero) ? nextTimestamp(delay)))
              })

  private def nextTimestamp(delay: FiniteDuration) =
    clock() + delay match {
      case at if delay >= 10.seconds => at.roundToNextSecond
      case at => at
    }
}

object RetryExecutor
{
  private def missingTryProblem(branchPath: BranchPath) =
    Problem.pure(s"Retry: branchPath does not denotes a 'try' statement: $branchPath")
}
