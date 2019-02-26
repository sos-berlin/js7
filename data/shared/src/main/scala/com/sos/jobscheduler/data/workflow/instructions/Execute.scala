package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.workflow.Instruction
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
sealed trait Execute extends Instruction

object Execute
{
  def apply(name: WorkflowJob.Name) = Named(name)

  def apply(workflowJob: WorkflowJob) = Anonymous(workflowJob)

  final case class Named(name: WorkflowJob.Name, defaultArguments: Map[String, String] = Map.empty) extends Execute {
    override def toString = s"execute $name"
  }
  object Named {
    implicit val jsonEncoder: ObjectEncoder[Named] = named =>
      JsonObject.fromIterable(
        ("name" -> named.name.asJson) ::
        named.defaultArguments.nonEmpty.thenList("defaultArguments" -> named.defaultArguments.asJson) :::
        Nil)
    implicit val jsonDecoder: Decoder[Named] = cursor =>
      for {
        name <- cursor.get[WorkflowJob.Name]("name")
        arguments <- cursor.getOrElse[Map[String, String]]("defaultArguments")(Map.empty)
      } yield Named(name, arguments)
  }

  final case class Anonymous(job: WorkflowJob) extends Execute {
    override def toString = s"execute '${job.executablePath}'"
  }

  implicit val jsonCodec = TypedJsonCodec[Execute](
    Subtype.named[Named]("Execute.Named"),
    Subtype.named(deriveCodec[Anonymous], "Execute.Anonymous"))
}
