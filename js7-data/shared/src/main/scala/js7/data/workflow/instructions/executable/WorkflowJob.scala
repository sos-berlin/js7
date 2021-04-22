package js7.data.workflow.instructions.executable

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.CirceUtilsChecked
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.generic.GenericString
import js7.base.io.process.ReturnCode
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.typeclasses.IsEmpty.syntax.toIsEmptyAllOps
import js7.data.agent.AgentPath
import js7.data.job.{CommandLineExecutable, Executable, InternalExecutable, JobResourcePath, PathExecutable, ScriptExecutable}
import js7.data.order.Outcome
import js7.data.value.{NamedValues, NumberValue, ValuePrinter}
import js7.data.workflow.WorkflowPrinter
import js7.data.workflow.instructions.ReturnCodeMeaning
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowJob private(
  agentId: AgentPath,
  executable: Executable,
  defaultArguments: NamedValues,
  jobResourcePaths: Seq[JobResourcePath] = Nil,
  returnCodeMeaning: ReturnCodeMeaning/*TODO Move to ProcessExecutable*/,
  taskLimit: Int,/*TODO Rename as parallelism*/
  sigkillDelay: Option[FiniteDuration]/*TODO Move to ProcessExecutable*/,
  failOnErrWritten: Boolean)
{
  def toOutcome(namedValues: NamedValues, returnCode: ReturnCode) =
    Outcome.Completed(
      success = returnCodeMeaning.isSuccess(returnCode),
      namedValues + ("returnCode" -> NumberValue(returnCode.number)))

  def isExecutableOnAgent(agentId: AgentPath): Boolean =
    this.agentId == agentId

  //override def toString = s"Job($argumentsString)"

  def argumentsString = s"agent=${agentId.string}, " +
    (executable match {
      case PathExecutable(o, env, v1Compatible) => s"executable=$o"
      case ScriptExecutable(o, env, v1Compatible) => s"script=$o"
      case CommandLineExecutable(expr, env) => "command=" + ValuePrinter.quoteString(expr.toString)
      case InternalExecutable(className, jobArguments, arguments) =>
        "internalJobClass=" + ValuePrinter.quoteString(className) ++
          (jobArguments.nonEmpty ?? ("jobArguments=" + WorkflowPrinter.namedValuesToString(jobArguments))) ++
          (arguments.nonEmpty ?? ("arguments=" + ValuePrinter.objectExpressionToString(arguments)))
    }) +
    (returnCodeMeaning match {
      case ReturnCodeMeaning.Default => ""
      case ReturnCodeMeaning.Success(returnCodes) => s", successReturnCodes=(${returnCodes.map(_.number) mkString ", "})"
      case ReturnCodeMeaning.Failure(returnCodes) => s", failureReturnCodes=(${returnCodes.map(_.number) mkString ", "})"
    })
}

object WorkflowJob
{
  val DefaultTaskLimit = 1

  def apply(
    agentId: AgentPath,
    executable: Executable,
    defaultArguments: NamedValues = Map.empty,
    jobResourcePaths: Seq[JobResourcePath] = Nil,
    returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
    taskLimit: Int = DefaultTaskLimit,
    sigkillDelay: Option[FiniteDuration] = None,
    failOnErrWritten: Boolean = false)
  : WorkflowJob =
    checked(agentId, executable, defaultArguments, jobResourcePaths, returnCodeMeaning, taskLimit, sigkillDelay,
      failOnErrWritten = failOnErrWritten
    ).orThrow

  def checked(
    agentId: AgentPath,
    executable: Executable,
    defaultArguments: NamedValues = Map.empty,
    jobResourcePaths: Seq[JobResourcePath] = Nil,
    returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
    taskLimit: Int = DefaultTaskLimit,
    sigkillDelay: Option[FiniteDuration] = None,
    failOnErrWritten: Boolean = false)
  : Checked[WorkflowJob] =
    for (_ <- jobResourcePaths.checkUniqueness) yield
      new WorkflowJob(
        agentId, executable, defaultArguments, jobResourcePaths, returnCodeMeaning,
        taskLimit, sigkillDelay, failOnErrWritten)

  final case class Name private(string: String) extends GenericString
  object Name extends GenericString.NameValidating[Name] {
    val Anonymous: Name = unchecked("")
    override val name = "WorkflowJob.Name"

    protected def unchecked(string: String) = new Name(string)
  }

  /** To be used in Workflow with known WorkflowId. */
  implicit val jsonEncoder: Encoder.AsObject[WorkflowJob] = workflowJob =>
    JsonObject(
      "agentId" -> workflowJob.agentId.asJson,
      "executable" -> workflowJob.executable.asJson,
      "defaultArguments" -> workflowJob.defaultArguments.??.asJson,
      "jobResourcePaths" -> workflowJob.jobResourcePaths.??.asJson,
      "returnCodeMeaning" -> ((workflowJob.returnCodeMeaning != ReturnCodeMeaning.Default) ? workflowJob.returnCodeMeaning).asJson,
      "taskLimit" -> workflowJob.taskLimit.asJson,
      "sigkillDelay" -> workflowJob.sigkillDelay.asJson,
      "failOnErrWritten" -> workflowJob.failOnErrWritten.?.asJson)

  implicit val jsonDecoder: Decoder[WorkflowJob] = cursor =>
    for {
      executable <- cursor.get[Executable]("executable")
      agentId <- cursor.get[AgentPath]("agentId")
      arguments <- cursor.getOrElse[NamedValues]("defaultArguments")(Map.empty)
      jobResourcePaths <- cursor.getOrElse[Seq[JobResourcePath]]("jobResourcePaths")(Nil)
      rc <- cursor.getOrElse[ReturnCodeMeaning]("returnCodeMeaning")(ReturnCodeMeaning.Default)
      taskLimit <- cursor.getOrElse[Int]("taskLimit")(DefaultTaskLimit)
      sigkillDelay <- cursor.get[Option[FiniteDuration]]("sigkillDelay")
      failOnErrWritten <- cursor.getOrElse[Boolean]("failOnErrWritten")(false)
      job <- checked(agentId, executable, arguments, jobResourcePaths, rc, taskLimit, sigkillDelay, failOnErrWritten)
        .toDecoderResult(cursor.history)
    } yield job
}
