package js7.proxy.javaapi.data.workflow.position

import io.vavr.control.{Either => VEither}
import js7.base.problem.Problem
import js7.data.workflow.position.WorkflowPosition
import js7.proxy.javaapi.data.common.JJsonable
import js7.proxy.javaapi.data.workflow.JWorkflowId

final case class JWorkflowPosition(asScala: WorkflowPosition)
extends JJsonable [JWorkflowPosition]
{
  protected type AsScala = WorkflowPosition
  protected def companion = JWorkflowPosition

  def workflowId = JWorkflowId(asScala.workflowId)

  def position = JPosition(asScala.position)
}

object JWorkflowPosition extends JJsonable.Companion[JWorkflowPosition]
{
  def of(workflowId: JWorkflowId, position: JPosition) =
    new JWorkflowPosition(WorkflowPosition(workflowId.asScala, position.asScala))

  override def fromJson(jsonString: String): VEither[Problem, JWorkflowPosition] =
    super.fromJson(jsonString)

  val jsonEncoder = WorkflowPosition.jsonEncoder
  val jsonDecoder = WorkflowPosition.jsonDecoder
}
