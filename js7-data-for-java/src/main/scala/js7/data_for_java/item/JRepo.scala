package js7.data_for_java.item

import io.vavr.control.{Either => VEither}
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.item.{Repo, VersionId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.data_for_java.vavr.VavrConverters._
import js7.data_for_java.workflow.{JWorkflow, JWorkflowId}
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

final class JRepo(asScala: Repo)
{
  /** The current `VersionId`.
    * @return `Optional.empty` iff the repo is empty. */
  @Nonnull
  def versionId: java.util.Optional[VersionId] =
    asScala.versions.headOption.toJava

  /** The `VersionId`s in reverse order of creation.
    * The first entry is the current `VersionId`,
    * and the last entry is the oldest `VersionId` .
    */
  @Nonnull
  def versionIds: java.util.Collection[VersionId] =
    asScala.versions.asJavaCollection

  @Nonnull
  def idToWorkflow(@Nonnull workflowId: JWorkflowId): VEither[Problem, JWorkflow] =
    asScala.idTo[Workflow](workflowId.asScala)
      .map(JWorkflow.apply)
      .toVavr

  @Nonnull
  def pathToWorkflow(@Nonnull workflowPath: WorkflowPath): VEither[Problem, JWorkflow] =
    asScala.pathTo(Workflow)(workflowPath)
      .map(JWorkflow.apply)
      .toVavr
}
