package js7.data.workflow.instructions

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.typeclasses.IsEmpty._
import js7.base.utils.typeclasses.IsEmpty.syntax._
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.value.NamedValues
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Instruction, Workflow}

/**
  * @author Joacim Zschimmer
  */
sealed trait Execute extends Instruction
{
  def defaultArguments: NamedValues

  override def reduceForAgent(agentId: AgentPath, workflow: Workflow) =
    if (isVisibleForAgent(agentId, workflow))
      this
    else
      Gap(sourcePos)
}

object Execute
{
  def apply(name: WorkflowJob.Name) =
    Named(name)

  def apply(name: WorkflowJob.Name, defaultArguments: NamedValues) =
    Named(name, defaultArguments)

  def apply(workflowJob: WorkflowJob) =
    Anonymous(workflowJob)

  def apply(workflowJob: WorkflowJob, defaultArguments: NamedValues) =
    Anonymous(workflowJob, defaultArguments)

  final case class Named(
    name: WorkflowJob.Name,
    defaultArguments: NamedValues = Map.empty,
    sourcePos: Option[SourcePos] = None)
  extends Execute
  {
    def withoutSourcePos = copy(sourcePos = None)

    override def isVisibleForAgent(agentId: AgentPath, workflow: Workflow) =
      workflow.findJob(name) // Should always find!
        .fold(_ => true, _ isExecutableOnAgent agentId)

    //override def toString = s"execute $name, defaultArguments=$defaultArguments"
  }
  object Named {
    implicit val jsonEncoder: Encoder.AsObject[Named] = named =>
      JsonObject(
        "jobName" -> named.name.asJson,
        "defaultArguments" -> named.defaultArguments.??.asJson,
        "sourcePos" -> named.sourcePos.asJson)
    implicit val jsonDecoder: Decoder[Named] = cursor =>
      for {
        name <- cursor.get[WorkflowJob.Name]("jobName")
        arguments <- cursor.getOrElse[NamedValues]("defaultArguments")(Map.empty)
        sourcePos <- cursor.get[Option[SourcePos]]("sourcePos")
      } yield Named(name, arguments, sourcePos)
  }

  final case class Anonymous(
    job: WorkflowJob,
    defaultArguments: NamedValues = Map.empty,
    sourcePos: Option[SourcePos] = None)
  extends Execute
  {
    def withoutSourcePos = copy(sourcePos = None)

    override def isVisibleForAgent(agentId: AgentPath, workflow: Workflow) =
      job isExecutableOnAgent agentId

    override def toString = s"execute(defaultArguments=$defaultArguments, $job)$sourcePosToString"
  }
  object Anonymous {
    // TODO Implement automatic removal of empty collections in JSON serialization/deserialization
    implicit val jsonEncoder: Encoder.AsObject[Anonymous] = o =>
      JsonObject(
        "job" -> o.job.asJson,
        "defaultArguments" -> o.defaultArguments.??.asJson,
        "sourcePos" -> o.sourcePos.asJson)
    implicit val jsonDecoder: Decoder[Anonymous] = cursor =>
      for {
        job <- cursor.get[WorkflowJob]("job")
        arguments <- cursor.getOrElse[NamedValues]("defaultArguments")(Map.empty)
        sourcePos <- cursor.get[Option[SourcePos]]("sourcePos")
      } yield Anonymous(job, arguments, sourcePos)
  }

  implicit val jsonCodec = TypedJsonCodec[Execute](
    Subtype.named[Named]("Execute.Named"),
    Subtype.named[Anonymous]("Execute.Anonymous"))
}
