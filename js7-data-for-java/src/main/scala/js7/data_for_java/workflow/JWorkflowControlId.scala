package js7.data_for_java.workflow

import io.vavr.control.Either as VEither
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.item.{UnsignedVersionedItemId, VersionId}
import js7.data.workflow.{WorkflowControlId, WorkflowControlPath}
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedVersionedItemId

@javaApi
final case class JWorkflowControlId(asScala: WorkflowControlId)
extends JJsonable[JWorkflowControlId] with JUnsignedVersionedItemId[WorkflowControlPath]:
  type AsScala = WorkflowControlId
  protected type ScalaPath = WorkflowControlPath
  protected def companion = JWorkflowControlId

  @Nonnull
  def path: WorkflowControlPath =
    asScala.path

@javaApi
object JWorkflowControlId extends JJsonable.Companion[JWorkflowControlId]:
  type AsScala = WorkflowControlId

  @javaApi @Nonnull @throws[RuntimeException]("on invalid syntax")
  def of(path: String, versionId: String): JWorkflowControlId =
    JWorkflowControlId(WorkflowControlPath(path) ~ versionId)

  @javaApi @Nonnull
  def of(path: WorkflowControlPath, versionId: VersionId): JWorkflowControlId =
    JWorkflowControlId(path ~ versionId)

  @javaApi @Nonnull
  def of(workflowId: JWorkflowId): JWorkflowControlId =
    JWorkflowControlId(WorkflowControlId(workflowId.asScala))

  @Nonnull
  override def fromJson(@Nonnull jsonString: String): VEither[Problem, JWorkflowControlId] =
    super.fromJson(jsonString)

  protected val jsonEncoder = UnsignedVersionedItemId.jsonEncoder[WorkflowControlPath]
  protected val jsonDecoder = UnsignedVersionedItemId.jsonDecoder[WorkflowControlPath]
