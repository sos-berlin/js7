package js7.data.workflow.instructions.executable

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.CirceUtilsChecked
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.generic.GenericString
import js7.base.io.process.KeyLogin
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.time.AdmissionTimeScheme
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.typeclasses.IsEmpty.syntax.toIsEmptyAllOps
import js7.data.agent.AgentPath
import js7.data.job.{CommandLineExecutable, Executable, InternalExecutable, JobResourcePath, PathExecutable, ShellScriptExecutable}
import js7.data.value.ValuePrinter
import js7.data.value.expression.Expression
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowJob private(
  agentPath: AgentPath,
  executable: Executable,
  defaultArguments: Map[String, Expression],
  jobResourcePaths: Seq[JobResourcePath],
  parallelism: Int,
  sigkillDelay: Option[FiniteDuration],
  timeout: Option[FiniteDuration],
  failOnErrWritten: Boolean,
  admissionTimeScheme: Option[AdmissionTimeScheme])
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
          (jobArguments.nonEmpty ?? ("jobArguments=" + ValuePrinter.nameToExpressionToString(jobArguments))) ++
          (arguments.nonEmpty ?? ("arguments=" + ValuePrinter.nameToExpressionToString(arguments)))
    })
}

object WorkflowJob
{
  val DefaultParallelism = 1

  def apply(
    agentPath: AgentPath,
    executable: Executable,
    defaultArguments: Map[String, Expression] = Map.empty,
    jobResourcePaths: Seq[JobResourcePath] = Nil,
    parallelism: Int = DefaultParallelism,
    sigkillDelay: Option[FiniteDuration] = None,
    timeout: Option[FiniteDuration] = None,
    login: Option[KeyLogin] = None,
    failOnErrWritten: Boolean = false,
    admissionTimeScheme: Option[AdmissionTimeScheme] = None)
  : WorkflowJob =
    checked(agentPath, executable, defaultArguments, jobResourcePaths, parallelism,
      sigkillDelay, timeout, failOnErrWritten = failOnErrWritten, admissionTimeScheme
    ).orThrow

  def checked(
    agentPath: AgentPath,
    executable: Executable,
    defaultArguments: Map[String, Expression] = Map.empty,
    jobResourcePaths: Seq[JobResourcePath] = Nil,
    parallelism: Int = DefaultParallelism,
    sigkillDelay: Option[FiniteDuration] = None,
    timeout: Option[FiniteDuration] = None,
    failOnErrWritten: Boolean = false,
    admissionTimeScheme: Option[AdmissionTimeScheme] = None)
  : Checked[WorkflowJob] =
    for (_ <- jobResourcePaths.checkUniqueness) yield
      new WorkflowJob(
        agentPath, executable, defaultArguments, jobResourcePaths,
        parallelism, sigkillDelay, timeout, failOnErrWritten, admissionTimeScheme)

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
      "timeout" -> workflowJob.timeout.asJson,
      "failOnErrWritten" -> workflowJob.failOnErrWritten.?.asJson,
      "admissionTimeScheme" -> workflowJob.admissionTimeScheme.asJson)

  implicit val jsonDecoder: Decoder[WorkflowJob] = cursor =>
    for {
      executable <- cursor.get[Executable]("executable")
      agentPath <- cursor.get[AgentPath]("agentPath")
      arguments <- cursor.getOrElse[Map[String, Expression]]("defaultArguments")(Map.empty)
      jobResourcePaths <- cursor.getOrElse[Seq[JobResourcePath]]("jobResourcePaths")(Nil)
      parallelism <- cursor.getOrElse[Int]("parallelism")(DefaultParallelism)
      sigkillDelay <- cursor.get[Option[FiniteDuration]]("sigkillDelay")
      timeout <- cursor.get[Option[FiniteDuration]]("timeout")
      failOnErrWritten <- cursor.getOrElse[Boolean]("failOnErrWritten")(false)
      admissionTimeScheme <- cursor.get[Option[AdmissionTimeScheme]]("admissionTimeScheme")
      job <- checked(agentPath, executable, arguments, jobResourcePaths, parallelism,
        sigkillDelay, timeout, failOnErrWritten, admissionTimeScheme
      ).toDecoderResult(cursor.history)
    } yield job
}
