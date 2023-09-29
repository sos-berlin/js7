package js7.data.job

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.syntax.*
import js7.data.workflow.position.*
import js7.data.workflow.{WorkflowId, WorkflowPath}

/**
  * @author Joacim Zschimmer
  */
sealed trait JobKey
{
  def name: String

  def workflowId: WorkflowId

  override def toString = s"Job:$name"
}

object JobKey
{
  def apply(workflowPosition: WorkflowPosition) =
    Anonymous(workflowPosition)

  def apply(workflowBranchPath: WorkflowBranchPath, name: WorkflowJob.Name) =
    Named(workflowBranchPath, name)

  def forTest: JobKey = forTest("TEST")

  def forTest(name: String) =
    Named(WorkflowBranchPath(WorkflowPath.NoId, Nil), WorkflowJob.Name(name))

  final case class Anonymous(workflowPosition: WorkflowPosition) extends JobKey {
    def name = workflowPosition.toString

    def workflowId = workflowPosition.workflowId
  }

  final case class Named(workflowBranchPath: WorkflowBranchPath, jobName: WorkflowJob.Name)
  extends JobKey {
    def name = s"$workflowBranchPath:${jobName.string}"

    def workflowId = workflowBranchPath.workflowId
  }

  implicit val jsonEncoder: Encoder.AsObject[JobKey] = {
    case Anonymous(WorkflowPosition(workflowId, position)) =>
      JsonObject(
        "workflowId" -> workflowId.asJson,
        "position" -> position.asJson)

    case Named(WorkflowBranchPath(workflowId, branchPath), name) =>
      JsonObject(
        "workflowId" -> workflowId.asJson,
        "branchPath" -> branchPath.??.asJson,
        "jobName" -> name.asJson)
  }

  implicit val jsonDecoder: Decoder[JobKey] =
    c => for {
      workflowId <- c.get[WorkflowId]("workflowId")
      jobKey <- {
        val c1 = c.downField("position")
        if (c1.succeeded)
          c1.as[Position].map(o => Anonymous(workflowId /: o))
        else
          for {
            branchPath <- c.getOrElse[BranchPath]("branchPath")(Nil)
            name <- c.get[WorkflowJob.Name]("jobName")
          } yield Named(WorkflowBranchPath(workflowId, branchPath), name)
      }
    } yield jobKey
}
