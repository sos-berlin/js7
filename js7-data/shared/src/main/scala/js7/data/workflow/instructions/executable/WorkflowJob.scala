package js7.data.workflow.instructions.executable

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.CirceUtilsChecked
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.generic.GenericString
import js7.base.io.process.KeyLogin
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.typeclasses.IsEmpty.syntax.toIsEmptyAllOps
import js7.data.agent.AgentPath
import js7.data.job.{CommandLineExecutable, Executable, InternalExecutable, JobResourcePath, PathExecutable, ShellScriptExecutable}
import js7.data.value.{NamedValues, ValuePrinter}
import js7.data.workflow.WorkflowPrinter
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowJob private(
  agentPath: AgentPath,
  executable: Executable,
  defaultArguments: NamedValues,
  jobResourcePaths: Seq[JobResourcePath],
  parallelism: Int,
  sigkillDelay: Option[FiniteDuration],
  failOnErrWritten: Boolean)
{
  def referencedJobResourcePaths =
    jobResourcePaths.view ++ executable.referencedJobResourcePaths

  def isExecutableOnAgent(agentPath: AgentPath): Boolean =
    this.agentPath == agentPath

  def argumentsString = s"agent=${agentPath.string}, " +
    (executable match {
      case PathExecutable(o, env, login, returnCodeMeansing, v1Compatible) => s"executable=$o"
      case ShellScriptExecutable(o, env, login, returnCodeMeansing, v1Compatible) => s"script=$o"
      case CommandLineExecutable(expr, login, returnCodeMeansing, env) => "command=" + ValuePrinter.quoteString(expr.toString)
      case InternalExecutable(className, jobArguments, arguments) =>
        "internalJobClass=" + ValuePrinter.quoteString(className) ++
          (jobArguments.nonEmpty ?? ("jobArguments=" + WorkflowPrinter.namedValuesToString(jobArguments))) ++
          (arguments.nonEmpty ?? ("arguments=" + ValuePrinter.nameToExpressionToString(arguments)))
    })
}

object WorkflowJob
{
  val DefaultParallelism = 1

  def apply(
    agentPath: AgentPath,
    executable: Executable,
    defaultArguments: NamedValues = Map.empty,
    jobResourcePaths: Seq[JobResourcePath] = Nil,
    parallelism: Int = DefaultParallelism,
    sigkillDelay: Option[FiniteDuration] = None,
    login: Option[KeyLogin] = None,
    failOnErrWritten: Boolean = false)
  : WorkflowJob =
    checked(agentPath, executable, defaultArguments, jobResourcePaths, parallelism, sigkillDelay,
      failOnErrWritten = failOnErrWritten
    ).orThrow

  def checked(
    agentPath: AgentPath,
    executable: Executable,
    defaultArguments: NamedValues = Map.empty,
    jobResourcePaths: Seq[JobResourcePath] = Nil,
    parallelism: Int = DefaultParallelism,
    sigkillDelay: Option[FiniteDuration] = None,
    failOnErrWritten: Boolean = false)
  : Checked[WorkflowJob] =
    for (_ <- jobResourcePaths.checkUniqueness) yield
      new WorkflowJob(
        agentPath, executable, defaultArguments, jobResourcePaths,
        parallelism, sigkillDelay, failOnErrWritten)

  final case class Name private(string: String) extends GenericString
  object Name extends GenericString.NameValidating[Name] {
    val Anonymous: Name = unchecked("")
    override val name = "WorkflowJob.Name"

    protected def unchecked(string: String) = new Name(string)
  }

  /** To be used in Workflow with known WorkflowId. */
  implicit val jsonEncoder: Encoder.AsObject[WorkflowJob] = workflowJob =>
    JsonObject(
      "agentPath" -> workflowJob.agentPath.asJson,
      "executable" -> workflowJob.executable.asJson,
      "defaultArguments" -> workflowJob.defaultArguments.??.asJson,
      "jobResourcePaths" -> workflowJob.jobResourcePaths.??.asJson,
      "parallelism" -> workflowJob.parallelism.asJson,
      "sigkillDelay" -> workflowJob.sigkillDelay.asJson,
      "failOnErrWritten" -> workflowJob.failOnErrWritten.?.asJson)

  implicit val jsonDecoder: Decoder[WorkflowJob] = cursor =>
    for {
      executable <- cursor.get[Executable]("executable")
      agentPath <- cursor.get[AgentPath]("agentPath")
      arguments <- cursor.getOrElse[NamedValues]("defaultArguments")(Map.empty)
      jobResourcePaths <- cursor.getOrElse[Seq[JobResourcePath]]("jobResourcePaths")(Nil)
      parallelism <- cursor.getOrElse[Int]("parallelism")(DefaultParallelism)
      sigkillDelay <- cursor.get[Option[FiniteDuration]]("sigkillDelay")
      failOnErrWritten <- cursor.getOrElse[Boolean]("failOnErrWritten")(false)
      job <- checked(agentPath, executable, arguments, jobResourcePaths, parallelism,
        sigkillDelay, failOnErrWritten
      ).toDecoderResult(cursor.history)
    } yield job
}
