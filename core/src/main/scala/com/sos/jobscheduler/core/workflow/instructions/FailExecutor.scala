package com.sos.jobscheduler.core.workflow.instructions

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.order.OrderEvent.{OrderDetachable, OrderFailed, OrderFailedCatchable, OrderFailedInFork, OrderStarted}
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.{Fail, Fork}
import com.sos.jobscheduler.data.workflow.position.Position

/**
  * @author Joacim Zschimmer
  */
object FailExecutor extends EventInstructionExecutor
{
  type Instr = Fail

  def toEvent(context: OrderContext, order: Order[Order.State], fail: Fail) =
    order.state match {
      case _: Order.Fresh =>
        Valid(Some(order.id <-: OrderStarted))

      case _: Order.Ready =>
        lazy val maybeErrorMessage = fail.errorMessage
          .map(o => context.makeScope(order).evalString(o).valueOr(_.toString))
        lazy val outcome = fail.returnCode match {
          case Some(returnCode) =>
            Outcome.Failed(maybeErrorMessage, returnCode)
          case None => order.lastOutcome match {
            case o: Outcome.NotSucceeded => o
            case o: Outcome.Succeeded => Outcome.Failed(maybeErrorMessage, o.returnCode, Map.empty)
          }
        }
        Valid(Some(order.id <-: (
          if (!fail.uncatchable)
            OrderFailedCatchable(outcome)
          else if (order.position.isInFork)
            OrderFailedInFork(outcome)
          else if (order.isAttached)
            OrderDetachable
          else
            OrderFailed(outcome))))

      case _: Order.FailedInFork =>
        if (order.isAttached)
          Valid(Some(order.id <-: OrderDetachable))
        else
          forkPositionOf(order.position).flatMap(forkPosition =>
            context.instruction(order.workflowId /: forkPosition) match {
              case fork: Fork =>
                ForkExecutor.tryJoinChildOrder(context, order, fork)

              case _ =>
                Valid(None)
            })

      case _ => Valid(None)
    }

  private[instructions] def forkPositionOf(childPosition: Position): Checked[Position] = {
    val forkBranchPath = childPosition.branchPath.reverse.dropWhile(o => !o.branchId.isFork).reverse
    if (forkBranchPath.isEmpty)
      Invalid(Problem.pure("Order is in state FailedInFork but not below a Fork instruction"))
    else
      Valid(forkBranchPath.init % forkBranchPath.last.nr)
  }
}
