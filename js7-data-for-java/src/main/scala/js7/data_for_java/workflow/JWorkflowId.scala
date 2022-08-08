package js7.data_for_java.workflow

import io.vavr.control.Either as VEither
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.item.{VersionId, VersionedItemId}
import js7.data.workflow.{WorkflowId, WorkflowPath}
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JVersionedItemId

@javaApi
final case class JWorkflowId(asScala: WorkflowId)
extends JJsonable[JWorkflowId] with JVersionedItemId[WorkflowPath]
{
  type AsScala = WorkflowId
  protected type ScalaPath = WorkflowPath
  protected def companion = JWorkflowId

  @Nonnull
  def path = asScala.path
}

@javaApi
object JWorkflowId extends JJsonable.Companion[JWorkflowId]
{
  @javaApi @Nonnull @throws[RuntimeException]("on invalid syntax")
  def of(path: String, versionId: String): JWorkflowId =
    JWorkflowId(WorkflowPath(path) ~ versionId)

  @javaApi @Nonnull
  def of(path: WorkflowPath, versionId: VersionId): JWorkflowId =
    JWorkflowId(path ~ versionId)

  @Nonnull
  override def fromJson(@Nonnull jsonString: String): VEither[Problem, JWorkflowId] =
    super.fromJson(jsonString)

  protected val jsonEncoder = VersionedItemId.jsonEncoder[WorkflowPath]
  protected val jsonDecoder = VersionedItemId.jsonDecoder[WorkflowPath]
}
