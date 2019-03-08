package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.Logger.ops.RichScalaLogger
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.order.Order
import com.sos.jobscheduler.data.order.OrderEvent.OrderRetrying
import com.sos.jobscheduler.data.workflow.instructions.{Retry, TryInstruction}
import com.sos.jobscheduler.data.workflow.position.TryBranchId
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class RetryExecutor(clock: () => Timestamp) extends EventInstructionExecutor
{
  type Instr = Retry

  private val logger = Logger(getClass)

  def toEvent(context: OrderContext, order: Order[Order.State], retry: Retry) =
    order.workflowPosition.position.nextRetryBranchPath
      .onProblem(p => logger.error(p))
      .flatMap(branchPath =>
        branchPath.parent.flatMap(parent =>
          context.instruction(order.workflowId /: parent) match {
            case _try: TryInstruction =>
              branchPath.lastOption.map(_.branchId) collect {
                case TryBranchId(index) => _try.retryDelay(index)
              }
            case _ => None
          })
        match {
          case None =>
            logger.error(s"Retry: branchPath does not denotes a 'try' statement: $branchPath")
            None
          case Some(delay) =>
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
