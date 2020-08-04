package js7.proxy.javaapi.data

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.workflow.{Workflow, WorkflowPath}

@javaApi
final case class JWorkflow(underlying: Workflow)
extends JInventoryItem[JWorkflow, WorkflowPath]
{
  protected type Underlying = Workflow

  def companion = JWorkflow

  def id = JWorkflowId(underlying.id)
}

@javaApi
object JWorkflow extends JJsonable.Companion[JWorkflow]
{
  override def fromJson(jsonString: String): VEither[Problem, JWorkflow] =
    super.fromJson(jsonString)

  def jsonEncoder = Workflow.jsonEncoder
  def jsonDecoder = Workflow.jsonDecoder
}
