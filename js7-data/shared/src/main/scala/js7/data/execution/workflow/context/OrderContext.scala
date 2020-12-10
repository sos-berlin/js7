package js7.data.execution.workflow.context

import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.implicitClass
import js7.data.lock.{LockId, LockState}
import js7.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import js7.data.value.expression.{Scope, ValueSearch}
import js7.data.value.{NamedValues, NumericValue, Value}
import js7.data.workflow.instructions.Instructions
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Instruction, Workflow, WorkflowId}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait OrderContext
{
  def idToOrder: OrderId => Checked[Order[Order.State]]

  def nameToLockState: LockId => Checked[LockState]

  def instruction(workflowPosition: WorkflowPosition): Instruction =
    idToWorkflow(workflowPosition.workflowId)
      .map(_.instruction(workflowPosition.position))
      .orThrow

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

  final def makeScope(order: Order[Order.State]): Checked[Scope] =
    idToWorkflow(order.workflowId)
      .map(OrderContext.makeScope(NamedValues.empty, order, _))
}

object OrderContext
{
  final def makeScope(
    highPriorityArguments: NamedValues,
    order: Order[Order.State],
    workflow: Workflow,
    default: NamedValues = NamedValues.empty): Scope =
    new Scope {
      private lazy val catchCount = Right(NumericValue(order.workflowPosition.position.catchCount))

      val symbolToValue = {
        case "catchCount" => catchCount
      }

      val findValue = {
        case ValueSearch(ValueSearch.Argument, ValueSearch.NamedValue(name)) =>
          Right(order.arguments.get(name))

        case ValueSearch(ValueSearch.LastOccurred, ValueSearch.NamedValue(name)) =>
          Right(
            highPriorityArguments.get(name)
              .orElse(
                order.historicOutcomes.reverseIterator
                  .collectFirst {
                    case HistoricOutcome(_, outcome: Outcome.Completed) if outcome.namedValues.contains(name) =>
                      outcome.namedValues(name)
                  })
              .orElse(order.arguments.get(name))
              .orElse(default.get(name)))

        case ValueSearch(ValueSearch.LastExecuted(positionSearch), what) =>
          order.historicOutcomes
            .reverseIterator
            .collectFirst {
              case HistoricOutcome(pos, outcome: Outcome.Completed) if workflow.positionMatchesSearch(pos, positionSearch) =>
                whatToValue(outcome, what)
            }
            .toChecked(Problem(s"There is no position in workflow that matches '$positionSearch'"))
      }
    }

  private def whatToValue(outcome: Outcome.Completed, what: ValueSearch.What): Option[Value] =
    what match {
      case ValueSearch.NamedValue(key) => outcome.namedValues.get(key)
    }
}
