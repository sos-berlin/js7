package com.sos.jobscheduler.core.workflow

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.core.workflow.OrderContext._
import com.sos.jobscheduler.data.expression.Evaluator.NumericValue
import com.sos.jobscheduler.data.expression.Scope
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.position.WorkflowPosition

/**
  * @author Joacim Zschimmer
  */
trait OrderContext
{
  def instruction(workflowPosition: WorkflowPosition): Instruction

  def idToOrder: PartialFunction[OrderId, Order[Order.State]]

  def childOrderEnded(order: Order[Order.State]): Boolean

  final def makeScope(order: Order[Order.State]): Scope = {
    new Scope {
      private lazy val returnCodeValue: Checked[NumericValue] = {
        val returnCode = order.lastOutcome match {
          case o: Outcome.Undisrupted => o.returnCode
          case _: Outcome.Disrupted => DisruptedReturnCode
        }
        Valid(NumericValue(returnCode.number))
      }

      private lazy val catchCount = Valid(NumericValue(order.workflowPosition.position.catchCount))

      val symbolToValue = {
        case "returnCode" => returnCodeValue
        case "catchCount" => catchCount
      }

      val variableNameToString = name => Valid(order.keyValues.get(name))
    }
  }
}

object OrderContext {
  private val DisruptedReturnCode = ReturnCode(-1)  // TODO Should we use this value ?
}
