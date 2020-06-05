package js7.data.workflow.instructions.executable

import js7.base.circeutils.CirceUtils.CirceUtilsChecked
import js7.base.generic.GenericString
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalazStyle.OptionRichBoolean
import js7.data.agent.AgentRefPath
import js7.data.job.{Executable, ExecutablePath, ExecutableScript, ReturnCode}
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.Outcome
import js7.data.workflow.instructions.ReturnCodeMeaning
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowJob private(
  agentRefPath: AgentRefPath,
  executable: Executable,
  defaultArguments: Map[String, String],
  returnCodeMeaning: ReturnCodeMeaning,
  taskLimit: Int)
{
  def toOrderProcessed(returnCode: ReturnCode, keyValues: Map[String, String]) =
    OrderProcessed(Outcome.Undisrupted(success = returnCodeMeaning.isSuccess(returnCode), returnCode, keyValues))

  def isExecutableOnAgent(agentRefPath: AgentRefPath): Boolean =
    this.agentRefPath == agentRefPath

  override def toString = s"Job($argumentsString)"

  def argumentsString = s"agent=${agentRefPath.string}, " +
    (executable match {
      case ExecutablePath(o) => s"executablePath=$o"
      case ExecutableScript(o) => s"script=$o"
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
    agentRefPath: AgentRefPath,
    executable: Executable,
    defaultArguments: Map[String, String] = Map.empty,
    returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
    taskLimit: Int = DefaultTaskLimit): WorkflowJob
  = checked(agentRefPath, executable, defaultArguments, returnCodeMeaning, taskLimit).orThrow

  def checked(
    agentRefPath: AgentRefPath,
    executable: Executable,
    defaultArguments: Map[String, String] = Map.empty,
    returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
    taskLimit: Int = DefaultTaskLimit): Checked[WorkflowJob]
  =
    if (agentRefPath.isAnonymous)
      Problem.pure("Anonymous AgentRef in Job?")
    else
      Right(new WorkflowJob(agentRefPath, executable, defaultArguments, returnCodeMeaning, taskLimit))

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
      ("agentRefPath" -> workflowJob.agentRefPath.asJson) ::
      ("executable" -> workflowJob.executable.asJson) ::
      workflowJob.defaultArguments.nonEmpty.thenList("defaultArguments" -> workflowJob.defaultArguments.asJson) :::
      (workflowJob.returnCodeMeaning != ReturnCodeMeaning.Default thenList ("returnCodeMeaning" -> workflowJob.returnCodeMeaning.asJson)) :::
      ("taskLimit" -> workflowJob.taskLimit.asJson) ::
      Nil)
  implicit val jsonDecoder: Decoder[WorkflowJob] = cursor =>
    for {
      //jobName <- cursor.get[Option[Name]]("jobName") map (_ getOrElse Name.Anonymous)
      executable <- cursor.get[Executable]("executable")
      agentRefPath <- cursor.get[AgentRefPath]("agentRefPath")
      arguments <- cursor.getOrElse[Map[String, String]]("defaultArguments")(Map.empty)
      rc <- cursor.getOrElse[ReturnCodeMeaning]("returnCodeMeaning")(ReturnCodeMeaning.Default)
      taskLimit <- cursor.get[Int]("taskLimit")
      job <- checked(agentRefPath, executable, arguments, rc, taskLimit).toDecoderResult(cursor.history)
    } yield job
}
