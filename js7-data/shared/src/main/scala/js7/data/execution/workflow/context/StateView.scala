package js7.data.execution.workflow.context

import js7.base.problem.Checked._
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits._
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.lock.{LockPath, LockState}
import js7.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import js7.data.value.expression.Expression.JobResourceSetting
import js7.data.value.expression.{Expression, Scope, ValueSearch}
import js7.data.value.{NumberValue, Value}
import js7.data.workflow.instructions.Instructions
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{Instruction, Workflow, WorkflowId}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait StateView
{
  def idToOrder: OrderId => Checked[Order[Order.State]]

  def pathToLockState: LockPath => Checked[LockState]

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

  final def workflowJob(workflowPosition: WorkflowPosition): Checked[WorkflowJob] =
    for {
      workflow <- idToWorkflow(workflowPosition.workflowId)
      job <- workflow.checkedWorkflowJob(workflowPosition.position)
    } yield job

  def childOrderEnded(order: Order[Order.State]): Boolean

  protected def idToWorkflow(id: WorkflowId): Checked[Workflow]

  final def makeScope(order: Order[Order.State])
  : Checked[Scope] =
    idToWorkflow(order.workflowId)
      .map(StateView.makeScope(order, _, jobResources = Nil))
}

object StateView
{
  final def makeScope(
    order: Order[Order.State],
    workflow: Workflow,
    jobResources: Seq[JobResource],
    default: PartialFunction[String, Value] = PartialFunction.empty)
  : Scope =
    new Scope {
      private val nameToMaybeDefault: String => Option[Value] = default.lift
      private lazy val catchCount = Right(NumberValue(
        order.workflowPosition.position.catchCount))
      private val pathToJobResource = jobResources.toKeyedMap(_.path)

      override def symbolToValue(symbol: String) = symbol match {
        case "catchCount" => Some(catchCount)
        case _ => None
      }

      override def findValue(search: ValueSearch) =
        Right(search match {
          case ValueSearch(ValueSearch.Argument, ValueSearch.Name(name)) =>
            argument(name)

          case ValueSearch(ValueSearch.LastOccurred, ValueSearch.Name(name)) =>
            order.historicOutcomes
              .reverseIterator
              .collectFirst {
                case HistoricOutcome(_, outcome: Outcome.Completed)
                  if outcome.namedValues.contains(name) =>
                  outcome.namedValues(name)
              }
              .orElse(argument(name))
              .orElse(nameToMaybeDefault(name))

          case ValueSearch(ValueSearch.LastExecuted(positionSearch), what) =>
            order.historicOutcomes
              .reverseIterator
              .collectFirst {
                case HistoricOutcome(pos, outcome: Outcome.Completed)
                  if workflow.positionMatchesSearch(pos, positionSearch) =>
                  whatToValue(outcome, what)
              }
              .flatten
        })

      private def argument(name: String): Option[Value] =
        order.arguments.get(name) orElse
          workflow.orderRequirements.defaultArgument(name)

      override def evalJobResourceSetting(setting: Expression.JobResourceSetting) =
        setting match {
          case JobResourceSetting(path: JobResourcePath, settingName) =>
            Some(
              pathToJobResource
                .rightOr(path, UnknownKeyProblem("JobResource", path.string))
                .flatMap(_.settings
                  .rightOr(settingName, UnknownKeyProblem("setting", s"$path:$settingName")))
                .flatMap(evaluator.eval))
          case _ =>
            None
        }
    }

  private def whatToValue(outcome: Outcome.Completed, what: ValueSearch.What): Option[Value] =
    what match {
      case ValueSearch.Name(key) => outcome.namedValues.get(key)
    }
}
