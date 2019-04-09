package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.core.workflow.instructions.RetryExecutor._
import com.sos.jobscheduler.data.order.OrderEvent.{OrderRetrying, OrderStopped}
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.{Retry, TryInstruction}
import com.sos.jobscheduler.data.workflow.position.{BranchPath, TryBranchId}
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class RetryExecutor(clock: () => Timestamp) extends EventInstructionExecutor
{
  type Instr = Retry

  def toEvent(context: OrderContext, order: Order[Order.State], retry: Retry) =
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
              Some(order.id <-: OrderStopped(Outcome.Disrupted(Problem.pure(s"Retry stopped because maxRetries=$maxRetries has been reached"))))
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
