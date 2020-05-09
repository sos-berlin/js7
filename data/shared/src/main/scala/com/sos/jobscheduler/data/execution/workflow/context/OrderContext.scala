package com.sos.jobscheduler.data.execution.workflow.context

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.data.execution.workflow.context.OrderContext._
import com.sos.jobscheduler.data.expression.Evaluator.{NumericValue, StringValue, Value}
import com.sos.jobscheduler.data.expression.{Scope, ValueSearch}
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Instructions
import com.sos.jobscheduler.data.workflow.position.WorkflowPosition
import com.sos.jobscheduler.data.workflow.{Instruction, Workflow, WorkflowId}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait OrderContext
{
  def idToOrder: PartialFunction[OrderId, Order[Order.State]]

  def instruction(workflowPosition: WorkflowPosition): Instruction

  def instruction_[A <: Instruction: ClassTag](workflowPosition: WorkflowPosition): Checked[Instruction] =
    instruction(workflowPosition) match {
      case o if implicitClass[A] isAssignableFrom o.getClass =>
        Right(o.asInstanceOf[A])
      case o =>
        Left(Problem(s"An Instruction '${Instructions.jsonCodec.classToName(implicitClass[A])}' " +
          s"is expected at position $workflowPosition, not: ${Instructions.jsonCodec.typeName(o)}"))
    }

  def childOrderEnded(order: Order[Order.State]): Boolean

  protected def idToWorkflow(id: WorkflowId): Checked[Workflow]

  final def makeScope(order: Order[Order.State]): Scope =
    new Scope {
      private lazy val catchCount = Right(NumericValue(order.workflowPosition.position.catchCount))

      val symbolToValue = {
        case "catchCount" => catchCount
      }

      val findValue = {
        case ValueSearch(ValueSearch.Argument, ValueSearch.KeyValue(name)) =>
          Right(order.arguments.get(name) map StringValue.apply)

        case ValueSearch(ValueSearch.Argument, ValueSearch.ReturnCode) =>
          Right(None)

        case ValueSearch(ValueSearch.LastOccurred, ValueSearch.KeyValue(name)) =>
          Right(
            order.historicOutcomes.reverseIterator
              .collectFirst {
                case HistoricOutcome(_, outcome: Outcome.Undisrupted) if outcome.keyValues.contains(name) =>
                  outcome.keyValues(name)
              }
              .orElse(order.arguments.get(name)) map StringValue.apply)

        case ValueSearch(ValueSearch.LastOccurred, ValueSearch.ReturnCode) =>
          Right(Some(NumericValue(outcomeToReturnCode(order.lastOutcome).number)))

        case ValueSearch(ValueSearch.LastExecuted(positionSearch), what) =>
          for {
            workflow <- idToWorkflow(order.workflowId)
            maybeValue <- order.historicOutcomes.reverseIterator
              .collectFirst {
                case HistoricOutcome(pos, outcome: Outcome.Undisrupted) if workflow.positionMatchesSearch(pos, positionSearch) =>
                  whatToValue(outcome, what)
              }
              .toChecked(Problem(s"There is no position in workflow that matches '$positionSearch'"))
          } yield maybeValue
      }
    }
}

object OrderContext
{
  private val DisruptedReturnCode = ReturnCode(-1)  // TODO Should we use this value ?

  private def whatToValue(outcome: Outcome.Undisrupted, what: ValueSearch.What): Option[Value] =
    what match {
      case ValueSearch.KeyValue(key) => outcome.keyValues.get(key) map StringValue.apply
      case ValueSearch.ReturnCode => Some(NumericValue(outcome.returnCode.number))
    }

  private def outcomeToReturnCode(outcome: Outcome): ReturnCode =
    outcome match {
      case o: Outcome.Undisrupted => o.returnCode
      case _: Outcome.Disrupted => DisruptedReturnCode
    }
}
