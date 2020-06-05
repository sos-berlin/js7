package js7.data.workflow.instructions

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.ScalazStyle.OptionRichBoolean
import js7.data.source.SourcePos
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.executable.WorkflowJob
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}

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
    implicit val jsonEncoder: Encoder.AsObject[Named] = named =>
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

    override def toString = s"execute (${job.argumentsString})"
  }

  implicit val jsonCodec = TypedJsonCodec[Execute](
    Subtype.named[Named]("Execute.Named"),
    Subtype.named(deriveCodec[Anonymous], "Execute.Anonymous"))
}
