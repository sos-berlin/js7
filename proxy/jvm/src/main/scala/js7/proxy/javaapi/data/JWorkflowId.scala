package js7.proxy.javaapi.data

import js7.base.annotation.javaApi
import js7.data.workflow.{WorkflowId, WorkflowPath}

@javaApi
final case class JWorkflowId(underlying: WorkflowId)
extends JFileBasedId[WorkflowPath]
{
  protected type ScalaPath = WorkflowPath
  protected type Underlying = WorkflowId

  def path = underlying.path
}

@javaApi
object JWorkflowId
{
  def of(path: String, commitId: String): JWorkflowId =
    JWorkflowId(WorkflowPath(path) ~ commitId)
}
