package js7.data.execution.workflow.context

import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.implicitClass
import js7.data.execution.workflow.context.OrderContext._
import js7.data.expression.Evaluator.{NumericValue, StringValue, Value}
import js7.data.expression.{Scope, ValueSearch}
import js7.data.job.ReturnCode
import js7.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import js7.data.workflow.instructions.Instructions
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Instruction, Workflow, WorkflowId}
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
                case HistoricOutcome(_, outcome: Outcome.Completed) if outcome.keyValues.contains(name) =>
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
                case HistoricOutcome(pos, outcome: Outcome.Completed) if workflow.positionMatchesSearch(pos, positionSearch) =>
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

  private def whatToValue(outcome: Outcome.Completed, what: ValueSearch.What): Option[Value] =
    what match {
      case ValueSearch.KeyValue(key) => outcome.keyValues.get(key) map StringValue.apply
      case ValueSearch.ReturnCode => Some(NumericValue(outcome.returnCode.number))
    }

  private def outcomeToReturnCode(outcome: Outcome): ReturnCode =
    outcome match {
      case o: Outcome.Completed => o.returnCode
      case _: Outcome.Disrupted => DisruptedReturnCode
    }
}
