package js7.proxy.javaapi.data

import js7.base.annotation.javaApi
import js7.data.filebased.VersionId
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
  @javaApi @throws[RuntimeException]("on invalid syntax")
  def of(path: String, versionId: String): JWorkflowId =
    JWorkflowId(WorkflowPath(path) ~ versionId)

  @javaApi
  def of(path: WorkflowPath, versionId: VersionId): JWorkflowId =
    JWorkflowId(path ~ versionId)
}
