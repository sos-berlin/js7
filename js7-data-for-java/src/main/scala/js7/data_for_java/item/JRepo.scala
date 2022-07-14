package js7.data_for_java.item

import io.vavr.control.Either as VEither
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.item.{Repo, VersionId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.data_for_java.vavr.VavrConverters.*
import js7.data_for_java.workflow.{JWorkflow, JWorkflowId}
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

final class JRepo(asScala: Repo)
{
  /** The current `VersionId`.
    * @return `Optional.empty` iff the repo is empty. */
  @Nonnull
  def versionId: java.util.Optional[VersionId] =
    asScala.versionIds.headOption.toJava

  /** The `VersionId`s in reverse order of creation.
    * The first entry is the current `VersionId`,
    * and the last entry is the oldest `VersionId` .
    */
  @Nonnull
  def versionIds: java.util.Collection[VersionId] =
    asScala.versionIds.asJavaCollection

  @deprecated("Renamed as idToCheckedWorkflow", "2.3")
  @Deprecated
  @Nonnull
  def idToWorkflow(@Nonnull workflowId: JWorkflowId): VEither[Problem, JWorkflow] =
    idToCheckedWorkflow(workflowId)

  @Nonnull
  def idToCheckedWorkflow(@Nonnull workflowId: JWorkflowId): VEither[Problem, JWorkflow] =
    asScala.idTo[Workflow](workflowId.asScala)
      .map(JWorkflow.apply)
      .toVavr

  @deprecated("Renamed as idToCheckedWorkflow", "2.3")
  @Deprecated
  @Nonnull
  def pathToWorkflow(@Nonnull workflowPath: WorkflowPath): VEither[Problem, JWorkflow] =
    pathToCheckedWorkflow(workflowPath)

  @Nonnull
  def pathToCheckedWorkflow(@Nonnull workflowPath: WorkflowPath): VEither[Problem, JWorkflow] =
    asScala.pathTo(Workflow)(workflowPath)
      .map(JWorkflow.apply)
      .toVavr
}
