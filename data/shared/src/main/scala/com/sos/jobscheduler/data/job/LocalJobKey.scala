package com.sos.jobscheduler.data.job

import com.sos.jobscheduler.data.workflow.Position
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import scala.language.implicitConversions

// NOT USED !!!
/**
  * @author Joacim Zschimmer
  */
sealed trait LocalJobKey

object LocalJobKey
{
  def apply(position: Position) = Anonymous(position)

  def apply(name: String) = Named(WorkflowJob.Name(name))

  def apply(name: WorkflowJob.Name) = Named(name)

  final case class Anonymous(position: Position) extends LocalJobKey
  final case class Named(name: WorkflowJob.Name) extends LocalJobKey

  implicit val jsonEncoder: Encoder[LocalJobKey] = {
    case Anonymous(position) ⇒ position.asJson  // Array
    case Named(name) ⇒ name.asJson  // String
  }

  implicit val jsonDecoder: Decoder[LocalJobKey] =
    _.as[Json].flatMap(o ⇒
      if (o.isArray)
        o.as[Position].map(Anonymous.apply)
      else
        o.as[WorkflowJob.Name].map(Named.apply))
}
