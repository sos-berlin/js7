package js7.data.workflow.instructions

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.agent.AgentPath
import js7.data.source.SourcePos
import js7.data.value.expression.Expression
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Instruction, Workflow}

/**
  * @author Joacim Zschimmer
  */
sealed trait Execute extends Instruction:
  def defaultArguments: Map[String, Expression]

  override def reduceForAgent(agentPath: AgentPath, workflow: Workflow) =
    if isVisibleForAgent(agentPath, workflow) then
      this
    else
      Gap(sourcePos)


object Execute:
  def apply(name: WorkflowJob.Name) =
    Named(name)

  def apply(name: WorkflowJob.Name, arguments: Map[String, Expression]) =
    Named(name, arguments)

  def apply(workflowJob: WorkflowJob) =
    Anonymous(workflowJob)

  final case class Named(
    name: WorkflowJob.Name,
    defaultArguments: Map[String, Expression] = Map.empty,
    sourcePos: Option[SourcePos] = None)
  extends Execute:
    override def instructionName = "Execute.Named"

    def withoutSourcePos = copy(sourcePos = None)

    override def isVisibleForAgent(agentPath: AgentPath, workflow: Workflow) =
      workflow.findJob(name) // Should always find!
        .fold(_ => true, _ isExecutableOnAgent agentPath)

    override def productPrefix = "Execute.Named"
    //override def toString = s"execute $name, defaultArguments=$defaultArguments"
  object Named:
    implicit val jsonEncoder: Encoder.AsObject[Named] = o =>
      JsonObject(
        "jobName" -> o.name.asJson,
        "defaultArguments" -> o.defaultArguments.??.asJson,
        "sourcePos" -> o.sourcePos.asJson)
    implicit val jsonDecoder: Decoder[Named] = cursor =>
      for
        name <- cursor.get[WorkflowJob.Name]("jobName")
        arguments <- cursor.getOrElse[Map[String, Expression]]("defaultArguments")(Map.empty)
        sourcePos <- cursor.get[Option[SourcePos]]("sourcePos")
      yield Named(name, arguments, sourcePos)

  final case class Anonymous(
    job: WorkflowJob,
    defaultArguments: Map[String, Expression] = Map.empty,
    sourcePos: Option[SourcePos] = None)
  extends Execute:
    override def instructionName = "Execute.Anonymous"

    def withoutSourcePos = copy(sourcePos = None)

    override def isVisibleForAgent(agentPath: AgentPath, workflow: Workflow) =
      job isExecutableOnAgent agentPath

    override def productPrefix = "Execute.Anonymous"
    override def toString = s"execute($job)$sourcePosToString"
  object Anonymous:
    // TODO Implement automatic removal of empty collections in JSON serialization/deserialization
    implicit val jsonEncoder: Encoder.AsObject[Anonymous] = o =>
      JsonObject(
        "job" -> o.job.asJson,
        "defaultArguments" -> o.defaultArguments.??.asJson,
        "sourcePos" -> o.sourcePos.asJson)
    implicit val jsonDecoder: Decoder[Anonymous] = cursor =>
      for
        job <- cursor.get[WorkflowJob]("job")
        args <- cursor.getOrElse[Map[String, Expression]]("defaultArguments")(Map.empty)
        sourcePos <- cursor.get[Option[SourcePos]]("sourcePos")
      yield Anonymous(job, args, sourcePos)

  implicit val jsonCodec: TypedJsonCodec[Execute] = TypedJsonCodec(
    Subtype.named1[Named]("Execute.Named"),
    Subtype.named1[Anonymous]("Execute.Anonymous"))
