package js7.data.workflow.instructions.executable

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.circeutils.CirceUtils.CirceUtilsChecked
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.generic.GenericString
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentId
import js7.data.job.{CommandLineExecutable, Executable, ExecutablePath, ExecutableScript, ReturnCode}
import js7.data.order.Outcome
import js7.data.value.{NamedValues, NumericValue, ValuePrinter}
import js7.data.workflow.instructions.ReturnCodeMeaning
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowJob private(
  agentId: AgentId,
  executable: Executable,
  defaultArguments: NamedValues,
  returnCodeMeaning: ReturnCodeMeaning,
  taskLimit: Int,
  sigkillAfter: Option[FiniteDuration])
{
  def toOutcome(namedValues: NamedValues, returnCode: ReturnCode) =
    Outcome.Completed(
      success = returnCodeMeaning.isSuccess(returnCode),
      namedValues + ("returnCode" -> NumericValue(returnCode.number)))

  def isExecutableOnAgent(agentId: AgentId): Boolean =
    this.agentId == agentId

  override def toString = s"Job($argumentsString)"

  def argumentsString = s"agent=${agentId.string}, " +
    (executable match {
      case ExecutablePath(o, env, v1Compatible) => s"executablePath=$o"
      case ExecutableScript(o, env, v1Compatible) => s"script=$o"
      case CommandLineExecutable(expr, env) => s"command=" + ValuePrinter.quoteString(expr.toString)
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
    agentId: AgentId,
    executable: Executable,
    defaultArguments: NamedValues = Map.empty,
    returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
    taskLimit: Int = DefaultTaskLimit,
    sigkillAfter: Option[FiniteDuration] = None)
  : WorkflowJob =
    checked(agentId, executable, defaultArguments, returnCodeMeaning, taskLimit, sigkillAfter).orThrow

  def checked(
    agentId: AgentId,
    executable: Executable,
    defaultArguments: NamedValues = Map.empty,
    returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
    taskLimit: Int = DefaultTaskLimit,
    sigkillAfter: Option[FiniteDuration] = None)
  : Checked[WorkflowJob] =
    Right(new WorkflowJob(agentId, executable, defaultArguments, returnCodeMeaning, taskLimit, sigkillAfter))

  final case class Name private(string: String) extends GenericString
  object Name extends GenericString.NameValidating[Name] {
    val Anonymous = Name.unchecked("")
    override val name = "WorkflowJob.Name"

    protected def unchecked(string: String) = new Name(string)
  }

  /** To be used in Workflow with known WorkflowId. */
  implicit val jsonEncoder: Encoder.AsObject[WorkflowJob] = workflowJob =>
    JsonObject.fromIterable(
      //(workflowJob.jobKey match {
      //  case JobKey.Named(_, jobName) => ("jobName" -> jobName.asJson) :: Nil
      //  case _ => Nil
      //}) :::
      ("agentId" -> workflowJob.agentId.asJson) ::
      ("executable" -> workflowJob.executable.asJson) ::
      workflowJob.defaultArguments.nonEmpty.thenList("defaultArguments" -> workflowJob.defaultArguments.asJson) :::
      (workflowJob.returnCodeMeaning != ReturnCodeMeaning.Default thenList ("returnCodeMeaning" -> workflowJob.returnCodeMeaning.asJson)) :::
      ("taskLimit" -> workflowJob.taskLimit.asJson) ::
      ("sigkillAfter" -> workflowJob.sigkillAfter.asJson) ::
      Nil)

  implicit val jsonDecoder: Decoder[WorkflowJob] = cursor =>
    for {
      //jobName <- cursor.get[Option[Name]]("jobName").map(_ getOrElse Name.Anonymous)
      executable <- cursor.get[Executable]("executable")
      agentId <- cursor.get[AgentId]("agentId")
      arguments <- cursor.getOrElse[NamedValues]("defaultArguments")(Map.empty)
      rc <- cursor.getOrElse[ReturnCodeMeaning]("returnCodeMeaning")(ReturnCodeMeaning.Default)
      taskLimit <- cursor.get[Int]("taskLimit")
      sigkillProcessesAfter <- cursor.get[Option[FiniteDuration]]("sigkillAfter")
      job <- checked(agentId, executable, arguments, rc, taskLimit, sigkillProcessesAfter)
        .toDecoderResult(cursor.history)
    } yield job
}
