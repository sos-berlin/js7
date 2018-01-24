package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.{OrderContext, Position, PositionInstruction, Workflow}
import io.circe.generic.JsonCodec
import scala.collection.immutable.{IndexedSeq, Seq}

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class IfReturnCode(returnCodes: Seq[ReturnCode], workflows: IndexedSeq[Workflow]) extends PositionInstruction {
  require(workflows.size >= 1 && workflows.size <= 2)

  def nextPosition(order: Order[Order.Processed], context: OrderContext) = {
    assert(order == context.idToOrder(order.id).withPosition(order.position))
    Some(order.outcome) collect {
      case Outcome.Good(okay) ⇒
        val index = if (returnCodes contains ReturnCode(if (okay) 0 else 1)) 0 else 1
        if (workflows.indices contains index)
          Position(Position.Parent(order.position.nr, index) :: Nil, 0)
        else
          order.position.increment  // Skip statement
    }
  }

  override def toString = s"IfReturnCode ${returnCodes map (_.number) mkString ", "} then $thenWorkflow" +
    (elseWorkflow map (w ⇒ s" else $w") getOrElse "")

  private def thenWorkflow: Workflow =
    workflows(0)

  private def elseWorkflow: Option[Workflow] =
    workflows.get(1)

  def workflowOption(branchId: Position.BranchId.Indexed): Option[Workflow] =
    workflows.get(branchId.number)
}
