package com.sos.jobscheduler.data.workflow.instructions

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Checked.ops._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.{OrderContext, Position, PositionInstruction, Workflow}
import io.circe.generic.extras._
import io.circe.generic.extras.defaults.defaultGenericConfiguration
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
@ConfiguredJsonCodec
final case class IfReturnCode(
  returnCodes: Seq[ReturnCode],
  @JsonKey("then")
  thenWorkflow: Workflow,
  @JsonKey("else")
  elseWorkflow: Option[Workflow] = None)
extends PositionInstruction
{
  def workflow(branchId: Position.BranchId.Indexed): Checked[Workflow] =
    branchId.number match {
      case 0 ⇒ Valid(thenWorkflow)
      case 1 ⇒ elseWorkflow toChecked Problem("This IfReturnCode has no 'else' branch")
      case _ ⇒ Invalid(Problem(s"Invalid index=${branchId.number} for IfReturnCode"))
    }

  def nextPosition(order: Order[Order.Processed], context: OrderContext) = {
    assert(order == context.idToOrder(order.id).withPosition(order.position))
    Some(order.state.outcome) collect {
      case Outcome.Succeeded(returnCode) ⇒
        val indexOption =
          if (returnCodes contains returnCode)
            Some(0)
          else if (elseWorkflow.isDefined)
            Some(1)
          else
            None
        indexOption match {
          case Some(index) ⇒ Position(Position.Parent(order.position.nr, index) :: Nil, 0)
          case None ⇒ order.position.increment  // Skip statement
        }
    }
  }

  override def toString = s"IfReturnCode ${returnCodes map (_.number) mkString ", "} then $thenWorkflow" +
    (elseWorkflow map (w ⇒ s" else $w") getOrElse "")
}

object IfReturnCode {
  intelliJuseImport(defaultGenericConfiguration)
}
