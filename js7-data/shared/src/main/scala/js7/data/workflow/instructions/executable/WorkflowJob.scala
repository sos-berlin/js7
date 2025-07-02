package js7.data.workflow.instructions.executable

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.toDecoderResult
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.generic.GenericString
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.time.AdmissionTimeScheme
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.agent.AgentPath
import js7.data.job.{Executable, JobResourcePath}
import js7.data.subagent.SubagentBundleId
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.{Expression, Scope}
import scala.collection.View
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowJob(
  agentPath: AgentPath,
  executable: Executable,
  defaultArguments: Map[String, Expression],
  subagentBundleId: Option[Expression],
  jobResourcePaths: Seq[JobResourcePath],
  processLimit: Int,
  sigkillDelay: Option[FiniteDuration],
  timeout: Option[FiniteDuration],
  killAtEndOfAdmissionPeriod: Boolean = false,
  failOnErrWritten: Boolean,
  admissionTimeScheme: Option[AdmissionTimeScheme],
  skipIfNoAdmissionStartForOrderDay: Boolean,
  isNotRestartable: Boolean):

  def referencedJobResourcePaths: View[JobResourcePath] =
    jobResourcePaths.view ++ executable.referencedJobResourcePaths

  def isExecutableOnAgent(agentPath: AgentPath): Boolean =
    this.agentPath == agentPath

  def checked: Checked[Unit] =
    subagentBundleId.fold(Checked.unit): expr =>
      if expr.isPure then
        // A pure expression can be checked beforehand
        expr.evalAsString(using Scope.empty).flatMap(SubagentBundleId.checked).rightAs(())
      else
        Checked.unit

  def isRestartable: Boolean =
    !isNotRestartable


object WorkflowJob:

  val DefaultProcessLimit = 1 // Or unlimited like AgentRef.processLimit ???

  def apply(
    agentPath: AgentPath,
    executable: Executable,
    defaultArguments: Map[String, Expression] = Map.empty,
    subagentBundleId: Option[Expression] = None,
    jobResourcePaths: Seq[JobResourcePath] = Nil,
    processLimit: Int = DefaultProcessLimit,
    sigkillDelay: Option[FiniteDuration] = None,
    timeout: Option[FiniteDuration] = None,
    killAtEndOfAdmissionPeriod: Boolean = false,
    failOnErrWritten: Boolean = false,
    admissionTimeScheme: Option[AdmissionTimeScheme] = None,
    skipIfNoAdmissionStartForOrderDay: Boolean = false,
    isNotRestartable: Boolean = false)
  : WorkflowJob =
    checked(agentPath, executable, defaultArguments, subagentBundleId, jobResourcePaths,
      processLimit, sigkillDelay, timeout,
      killAtEndOfAdmissionPeriod = killAtEndOfAdmissionPeriod,
      failOnErrWritten = failOnErrWritten,
      admissionTimeScheme, skipIfNoAdmissionStartForOrderDay, isNotRestartable
    ).orThrow

  def checked(
    agentPath: AgentPath,
    executable: Executable,
    defaultArguments: Map[String, Expression] = Map.empty,
    subagentBundleId: Option[Expression] = None,
    jobResourcePaths: Seq[JobResourcePath] = Nil,
    processLimit: Int = DefaultProcessLimit,
    sigkillDelay: Option[FiniteDuration] = None,
    timeout: Option[FiniteDuration] = None,
    killAtEndOfAdmissionPeriod: Boolean = false,
    failOnErrWritten: Boolean = false,
    admissionTimeScheme: Option[AdmissionTimeScheme] = None,
    skipIfNoAdmissionStartForOrderDay: Boolean = false,
    isNotRestartable: Boolean = false)
  : Checked[WorkflowJob] =
    for _ <- jobResourcePaths.checkUniqueness yield
      new WorkflowJob(
        agentPath, executable, defaultArguments, subagentBundleId, jobResourcePaths,
        processLimit, sigkillDelay, timeout,
        killAtEndOfAdmissionPeriod = killAtEndOfAdmissionPeriod,
        failOnErrWritten = failOnErrWritten,
        admissionTimeScheme, skipIfNoAdmissionStartForOrderDay, isNotRestartable)

  final case class Name private(string: String) extends GenericString
  object Name extends GenericString.NameValidating[Name]:
    val Anonymous: Name = unchecked("")
    override val name = "WorkflowJob.Name"

    protected def unchecked(string: String) = new Name(string)

  /** To be used in Workflow with known WorkflowId. */
  implicit val jsonEncoder: Encoder.AsObject[WorkflowJob] = workflowJob =>
    JsonObject(
      "agentPath" -> workflowJob.agentPath.asJson,
      "subagentBundleIdExpr" -> workflowJob.subagentBundleId.asJson,
      "executable" -> workflowJob.executable.asJson,
      "defaultArguments" -> workflowJob.defaultArguments.??.asJson,
      "jobResourcePaths" -> workflowJob.jobResourcePaths.??.asJson,
      "processLimit" -> workflowJob.processLimit.asJson,
      "sigkillDelay" -> workflowJob.sigkillDelay.asJson,
      "timeout" -> workflowJob.timeout.asJson,
      "killAtEndOfAdmissionPeriod" -> (workflowJob.killAtEndOfAdmissionPeriod ? true).asJson,
      "failOnErrWritten" -> workflowJob.failOnErrWritten.?.asJson,
      "admissionTimeScheme" -> workflowJob.admissionTimeScheme.asJson,
      "skipIfNoAdmissionStartForOrderDay" -> workflowJob.skipIfNoAdmissionStartForOrderDay.?.asJson,
      "isNotRestartable" -> workflowJob.isNotRestartable.?.asJson)

  implicit val jsonDecoder: Decoder[WorkflowJob] = c =>
    for
      executable <- c.get[Executable]("executable")
      subagentBundleId <-
        c.get[Option[SubagentBundleId]]("subagentBundleId")
          .flatMap:
            case Some(id) => Right(Some(id))
            case None => c.get[Option[SubagentBundleId]]("subagentSelectionId") // COMPATIBLE with v2.7.1
          .flatMap:
            case Some(id) => Right(Some(StringConstant(id.string)))
            case None =>
              c.get[Option[Expression]]("subagentBundleIdExpr").flatMap:
                case Some(expr) => Right(Some(expr))
                case None => c.get[Option[Expression]]("subagentBundleIdExpr") // COMPATIBLE with v2.7.1
      agentPath <- c.get[AgentPath]("agentPath")
      arguments <- c.getOrElse[Map[String, Expression]]("defaultArguments")(Map.empty)
      jobResourcePaths <- c.getOrElse[Seq[JobResourcePath]]("jobResourcePaths")(Nil)
      maybeProcessLimit <- c.get[Option[Int]]("processLimit")
      maybeProcessLimit <- maybeProcessLimit
        // COMPATIBLE with 2.5.5, 2.6.2
        .fold(c.getOrElse[Int]("parallelism")(DefaultProcessLimit))(Right(_))
      sigkillDelay <- c.get[Option[FiniteDuration]]("sigkillDelay")
      timeout <- c.get[Option[FiniteDuration]]("timeout")
      killAtEndOfAdmissionPeriod <- c.getOrElse[Boolean]("killAtEndOfAdmissionPeriod")(false)
      failOnErrWritten <- c.getOrElse[Boolean]("failOnErrWritten")(false)
      admissionTimeScheme <- c.get[Option[AdmissionTimeScheme]]("admissionTimeScheme")
      skipIfNoAdmissionStartForOrderDay <-
        c.get[Option[Boolean]]("skipIfNoAdmissionStartForOrderDay").flatMap:
          case Some(o) => Right(o)
          case None => // COMPATIBLE with v2.4
            c.getOrElse[Boolean]("skipIfNoAdmissionForOrderDay")(false)
      isNotRestartable <- c.getOrElse[Boolean]("isNotRestartable")(false)
      job <- checked(agentPath, executable, arguments, subagentBundleId, jobResourcePaths,
        maybeProcessLimit, sigkillDelay, timeout,
        killAtEndOfAdmissionPeriod = killAtEndOfAdmissionPeriod,
        failOnErrWritten = failOnErrWritten,
        admissionTimeScheme,
        skipIfNoAdmissionStartForOrderDay, isNotRestartable
      ).toDecoderResult(c.history)
    yield
      job
