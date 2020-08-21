package js7.proxy.javaapi.data

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.proxy.javaapi.data.workflow.position.JPosition
import js7.proxy.javaapi.utils.VavrConverters._

@javaApi
final case class JWorkflow(underlying: Workflow)
extends JInventoryItem[JWorkflow, WorkflowPath]
{
  protected type Underlying = Workflow

  def companion = JWorkflow

  def id = JWorkflowId(underlying.id)

  def checkedJobName(position: JPosition): VEither[Problem, WorkflowJob.Name] =
    underlying.checkedExecute(position.underlying)
      .flatMap {
        case named: Execute.Named => Right(named.name)
        case _ => Left(Problem(s"Job at position $position does not have a name"))
      }
      .toVavr
}

@javaApi
object JWorkflow extends JJsonable.Companion[JWorkflow]
{
  override def fromJson(jsonString: String): VEither[Problem, JWorkflow] =
    super.fromJson(jsonString)

  def jsonEncoder = Workflow.jsonEncoder
  def jsonDecoder = Workflow.jsonDecoder
}
