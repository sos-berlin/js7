package js7.data.workflow.instructions.executable

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.CirceUtilsChecked
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.generic.GenericString
import js7.base.io.process.KeyLogin
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.time.AdmissionTimeScheme
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.toIsEmptyAllOps
import js7.data.agent.AgentPath
import js7.data.job.{CommandLineExecutable, Executable, InternalExecutable, JobResourcePath, PathExecutable, ShellScriptExecutable}
import js7.data.subagent.SubagentSelectionId
import js7.data.value.ValuePrinter
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.{Expression, Scope}
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowJob(
  agentPath: AgentPath,
  executable: Executable,
  defaultArguments: Map[String, Expression],
  subagentSelectionId: Option[Expression],
  jobResourcePaths: Seq[JobResourcePath],
  parallelism: Int,
  sigkillDelay: Option[FiniteDuration],
  timeout: Option[FiniteDuration],
  failOnErrWritten: Boolean,
  admissionTimeScheme: Option[AdmissionTimeScheme],
  skipIfNoAdmissionStartForOrderDay: Boolean)
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
      case InternalExecutable(className, script, jobArguments, arguments) =>
        "internalJobClass=" + ValuePrinter.quoteString(className) ++
          (jobArguments.nonEmpty ?? ("jobArguments=" + ValuePrinter.nameToExpressionToString(jobArguments))) ++
          (arguments.nonEmpty ?? ("arguments=" + ValuePrinter.nameToExpressionToString(arguments)))
    })

  def checked: Checked[Unit] =
    subagentSelectionId.fold(Checked.unit)(expr =>
      if expr.isConstant then
        expr.evalAsString(Scope.empty).flatMap(SubagentSelectionId.checked).rightAs(())
      else
        Checked.unit)
}

object WorkflowJob
{
  val DefaultParallelism = 1

  def apply(
    agentPath: AgentPath,
    executable: Executable,
    defaultArguments: Map[String, Expression] = Map.empty,
    subagentSelectionId: Option[Expression] = None,
    jobResourcePaths: Seq[JobResourcePath] = Nil,
    parallelism: Int = DefaultParallelism,
    sigkillDelay: Option[FiniteDuration] = None,
    timeout: Option[FiniteDuration] = None,
    login: Option[KeyLogin] = None,
    failOnErrWritten: Boolean = false,
    admissionTimeScheme: Option[AdmissionTimeScheme] = None,
    skipIfNoAdmissionStartForOrderDay: Boolean = false)
  : WorkflowJob =
    checked(agentPath, executable, defaultArguments, subagentSelectionId, jobResourcePaths,
      parallelism, sigkillDelay, timeout, failOnErrWritten = failOnErrWritten,
      admissionTimeScheme, skipIfNoAdmissionStartForOrderDay
    ).orThrow

  def checked(
    agentPath: AgentPath,
    executable: Executable,
    defaultArguments: Map[String, Expression] = Map.empty,
    subagentSelectionId: Option[Expression] = None,
    jobResourcePaths: Seq[JobResourcePath] = Nil,
    parallelism: Int = DefaultParallelism,
    sigkillDelay: Option[FiniteDuration] = None,
    timeout: Option[FiniteDuration] = None,
    failOnErrWritten: Boolean = false,
    admissionTimeScheme: Option[AdmissionTimeScheme] = None,
    skipIfNoAdmissionStartForOrderDay: Boolean = false)
  : Checked[WorkflowJob] =
    for _ <- jobResourcePaths.checkUniqueness yield
      new WorkflowJob(
        agentPath, executable, defaultArguments, subagentSelectionId, jobResourcePaths,
        parallelism, sigkillDelay, timeout, failOnErrWritten,
        admissionTimeScheme, skipIfNoAdmissionStartForOrderDay)

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
      "subagentSelectionIdExpr" -> workflowJob.subagentSelectionId.asJson,
      "executable" -> workflowJob.executable.asJson,
      "defaultArguments" -> workflowJob.defaultArguments.??.asJson,
      "jobResourcePaths" -> workflowJob.jobResourcePaths.??.asJson,
      "parallelism" -> workflowJob.parallelism.asJson,
      "sigkillDelay" -> workflowJob.sigkillDelay.asJson,
      "timeout" -> workflowJob.timeout.asJson,
      "failOnErrWritten" -> workflowJob.failOnErrWritten.?.asJson,
      "admissionTimeScheme" -> workflowJob.admissionTimeScheme.asJson,
      "skipIfNoAdmissionStartForOrderDay" -> workflowJob.skipIfNoAdmissionStartForOrderDay.?.asJson)

  implicit val jsonDecoder: Decoder[WorkflowJob] = c =>
    for
      executable <- c.get[Executable]("executable")
      subagentSelectionId <-
        c.get[Option[SubagentSelectionId]]("subagentSelectionId")
          .flatMap {
            case Some(id) => Right(Some(StringConstant(id.string)))
            case None => c.get[Option[Expression]]("subagentSelectionIdExpr")
          }
      agentPath <- c.get[AgentPath]("agentPath")
      arguments <- c.getOrElse[Map[String, Expression]]("defaultArguments")(Map.empty)
      jobResourcePaths <- c.getOrElse[Seq[JobResourcePath]]("jobResourcePaths")(Nil)
      parallelism <- c.getOrElse[Int]("parallelism")(DefaultParallelism)
      sigkillDelay <- c.get[Option[FiniteDuration]]("sigkillDelay")
      timeout <- c.get[Option[FiniteDuration]]("timeout")
      failOnErrWritten <- c.getOrElse[Boolean]("failOnErrWritten")(false)
      admissionTimeScheme <- c.get[Option[AdmissionTimeScheme]]("admissionTimeScheme")
      skipIfNoAdmissionStartForOrderDay <-
        c.get[Option[Boolean]]("skipIfNoAdmissionStartForOrderDay").flatMap {
          case Some(o) => Right(o)
          case None => // COMPATIBLE with v2.4
            c.getOrElse[Boolean]("skipIfNoAdmissionForOrderDay")(false)
        }
      job <- checked(agentPath, executable, arguments, subagentSelectionId, jobResourcePaths,
        parallelism, sigkillDelay, timeout, failOnErrWritten, admissionTimeScheme,
        skipIfNoAdmissionStartForOrderDay
      ).toDecoderResult(c.history)
    yield job
}
