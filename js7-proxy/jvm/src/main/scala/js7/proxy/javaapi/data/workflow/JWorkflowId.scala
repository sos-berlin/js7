package js7.proxy.javaapi.data.workflow

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.item.{ItemId, VersionId}
import js7.data.workflow.{WorkflowId, WorkflowPath}
import js7.proxy.javaapi.data.common.JJsonable
import js7.proxy.javaapi.data.item.JItemId

@javaApi
final case class JWorkflowId(underlying: WorkflowId)
extends JJsonable[JWorkflowId] with JItemId[WorkflowPath]
{
  protected type Underlying = WorkflowId
  protected type ScalaPath = WorkflowPath
  protected def companion = JWorkflowId

  def path = underlying.path
}

@javaApi
object JWorkflowId extends JJsonable.Companion[JWorkflowId]
{
  @javaApi @throws[RuntimeException]("on invalid syntax")
  def of(path: String, versionId: String): JWorkflowId =
    JWorkflowId(WorkflowPath(path) ~ versionId)

  @javaApi
  def of(path: WorkflowPath, versionId: VersionId): JWorkflowId =
    JWorkflowId(path ~ versionId)

  override def fromJson(jsonString: String): VEither[Problem, JWorkflowId] =
    super.fromJson(jsonString)

  val jsonEncoder = ItemId.jsonEncoder[WorkflowPath]
  val jsonDecoder = ItemId.jsonDecoder[WorkflowPath]
}
