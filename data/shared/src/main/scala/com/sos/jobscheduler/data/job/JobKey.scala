package com.sos.jobscheduler.data.job

import cats.syntax.either._
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.{Position, WorkflowPosition}
import com.sos.jobscheduler.data.workflow.{WorkflowId, WorkflowPath}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
sealed trait JobKey {
  //def toLocalJobKey: LocalJobKey
}

object JobKey {
  def apply(workflowPosition: WorkflowPosition) =
    Anonymous(workflowPosition)

  def apply(workflowId: WorkflowId, name: WorkflowJob.Name) =
    Named(workflowId, name)

  def forTest: JobKey = forTest("TEST")

  def forTest(name: String) = Named(WorkflowPath.NoId, WorkflowJob.Name(name))

  final case class Anonymous(workflowPosition: WorkflowPosition) extends JobKey {
    def toLocalJobKey = LocalJobKey.Anonymous(workflowPosition.position)
  }

  final case class Named(workflowId: WorkflowId, name: WorkflowJob.Name) extends JobKey {
    def toLocalJobKey = LocalJobKey.Named(name)
  }

  implicit val jsonEncoder: ObjectEncoder[JobKey] = {
    case Anonymous(WorkflowPosition(workflowId, position)) ⇒
      JsonObject("workflowId" → workflowId.asJson, "position" → position.asJson)

    case Named(workflowId, name) ⇒
      JsonObject("workflowId" → workflowId.asJson, "name" → name.asJson)
  }

  implicit val jsonDecoder: Decoder[JobKey] =
    c ⇒ for {
      workflowId ← c.get[WorkflowId]("workflowId")
      jobKey ← c.get[Position]("position").map(o ⇒ Anonymous(workflowId /: o))
        .orElse(
          c.get[WorkflowJob.Name]("name").map(o ⇒ Named(workflowId, o)))
    } yield jobKey
}
