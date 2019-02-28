package com.sos.jobscheduler.data.job

import cats.syntax.either._
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.{Position, WorkflowPosition, _}
import com.sos.jobscheduler.data.workflow.{WorkflowId, WorkflowPath}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
sealed trait JobKey

object JobKey
{
  def apply(workflowPosition: WorkflowPosition) =
    Anonymous(workflowPosition)

  def apply(workflowBranchPath: WorkflowBranchPath, name: WorkflowJob.Name) =
    Named(workflowBranchPath, name)

  def forTest: JobKey = forTest("TEST")

  def forTest(name: String) = Named(WorkflowBranchPath(WorkflowPath.NoId, Nil), WorkflowJob.Name(name))

  final case class Anonymous(workflowPosition: WorkflowPosition) extends JobKey {
    override def toString = s"JobKey($workflowPosition)"
  }

  final case class Named(workflowBranchPath: WorkflowBranchPath, name: WorkflowJob.Name) extends JobKey {
    override def toString = s"JobKey($workflowBranchPath:${name.string})"
  }

  implicit val jsonEncoder: ObjectEncoder[JobKey] = {
    case Anonymous(WorkflowPosition(workflowId, position)) =>
      JsonObject(
        "workflowId" -> workflowId.asJson,
        "position" -> position.asJson)

    case Named(WorkflowBranchPath(workflowId, branchPath), name) =>
      JsonObject(
        "workflowId" -> workflowId.asJson,
        "branchPath" -> (branchPath.nonEmpty ? branchPath).asJson,
        "name" -> name.asJson)
  }

  implicit val jsonDecoder: Decoder[JobKey] =
    c => for {
      workflowId <- c.get[WorkflowId]("workflowId")
      jobKey <- c.get[Position]("position").map(o => Anonymous(workflowId /: o))
        .orElse(
          for {
            branchPath <- c.get[Option[BranchPath]]("branchPath") map (_ getOrElse Nil)
            name <- c.get[WorkflowJob.Name]("name")
          } yield Named(WorkflowBranchPath(workflowId, branchPath), name))
    } yield jobKey
}
