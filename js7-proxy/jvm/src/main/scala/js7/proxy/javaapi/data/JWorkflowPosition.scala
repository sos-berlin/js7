package js7.proxy.javaapi.data

import io.vavr.control.{Either => VEither}
import js7.base.problem.Problem
import js7.data.workflow.position.WorkflowPosition
import js7.proxy.javaapi.data.workflow.position.JPosition

final case class JWorkflowPosition(underlying: WorkflowPosition)
extends JJsonable [JWorkflowPosition]
{
  protected type Underlying = WorkflowPosition
  protected def companion = JWorkflowPosition

  def workflowId = JWorkflowId(underlying.workflowId)

  def position = JPosition(underlying.position)
}

object JWorkflowPosition extends JJsonable.Companion[JWorkflowPosition]
{
  def of(workflowId: JWorkflowId, position: JPosition) =
    new JWorkflowPosition(WorkflowPosition(workflowId.underlying, position.underlying))

  override def fromJson(jsonString: String): VEither[Problem, JWorkflowPosition] =
    super.fromJson(jsonString)

  val jsonEncoder = WorkflowPosition.jsonEncoder
  val jsonDecoder = WorkflowPosition.jsonDecoder
}
