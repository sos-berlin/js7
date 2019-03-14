package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.source.SourcePos
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

  final case class Named(name: WorkflowJob.Name, defaultArguments: Map[String, String] = Map.empty,
    sourcePos: Option[SourcePos] = None)
  extends Execute
  {
    def withoutSourcePos = copy(sourcePos = None)

    override def toString = s"execute $name"
  }
  object Named {
    implicit val jsonEncoder: ObjectEncoder[Named] = named =>
      JsonObject.fromIterable(
        ("jobName" -> named.name.asJson) ::
        named.defaultArguments.nonEmpty.thenList("defaultArguments" -> named.defaultArguments.asJson) :::
        named.sourcePos.toList.map(o => "sourcePos" -> o.asJson))
    implicit val jsonDecoder: Decoder[Named] = cursor =>
      for {
        name <- cursor.get[WorkflowJob.Name]("jobName")
        arguments <- cursor.getOrElse[Map[String, String]]("defaultArguments")(Map.empty)
        sourcePos <- cursor.get[Option[SourcePos]]("sourcePos")
      } yield Named(name, arguments, sourcePos)
  }

  final case class Anonymous(job: WorkflowJob, sourcePos: Option[SourcePos] = None)
  extends Execute
  {
    def withoutSourcePos = copy(sourcePos = None)

    override def toString = s"execute executable='${job.executablePath}', agent='${job.agentRefPath.string}'"
  }

  implicit val jsonCodec = TypedJsonCodec[Execute](
    Subtype.named[Named]("Execute.Named"),
    Subtype.named(deriveCodec[Anonymous], "Execute.Anonymous"))
}
