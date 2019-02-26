package com.sos.jobscheduler.data.workflow.instructions.executable

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils.CirceUtilsChecked
import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.OrderProcessed
import com.sos.jobscheduler.data.order.Outcome
import com.sos.jobscheduler.data.workflow.instructions.ReturnCodeMeaning
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowJob private(
  agentRefPath: AgentRefPath,
  executablePath: ExecutablePath,
  defaultArguments: Map[String, String],
  returnCodeMeaning: ReturnCodeMeaning,
  taskLimit: Int)
{
  def toOrderProcessed(variablesDiff: MapDiff[String, String], returnCode: ReturnCode) =
    OrderProcessed(variablesDiff, Outcome.Undisrupted(returnCode, success = returnCodeMeaning.isSuccess(returnCode)))

  def isExecutableOnAgent(agentRefPath: AgentRefPath): Boolean =
    this.agentRefPath == agentRefPath

  override def toString = s"Job(agent=${agentRefPath.string}, executable=${executablePath.string}" + (
    returnCodeMeaning match {
      case ReturnCodeMeaning.Default ⇒ ""
      case ReturnCodeMeaning.Success(returnCodes) ⇒ s", successReturnCodes=(${returnCodes.map(_.number) mkString ", "})"
      case ReturnCodeMeaning.Failure(returnCodes) ⇒ s", failureReturnCodes=(${returnCodes.map(_.number) mkString ", "})"
    }) +
    ")"
}

object WorkflowJob
{
  val DefaultTaskLimit = 1

  def apply(
    agentRefPath: AgentRefPath,
    executablePath: ExecutablePath,
    defaultArguments: Map[String, String] = Map.empty,
    returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
    taskLimit: Int = DefaultTaskLimit): WorkflowJob
  = checked(agentRefPath, executablePath, defaultArguments, returnCodeMeaning, taskLimit).orThrow

  def checked(
    agentRefPath: AgentRefPath,
    executablePath: ExecutablePath,
    defaultArguments: Map[String, String] = Map.empty,
    returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
    taskLimit: Int = DefaultTaskLimit): Checked[WorkflowJob]
  =
    if (agentRefPath.isAnonymous)
      Problem.pure("Anonymous AgentRef in Job?")
    else
      Valid(new WorkflowJob(agentRefPath, executablePath, defaultArguments, returnCodeMeaning, taskLimit))

  final case class Name private(string: String) extends GenericString
  object Name extends GenericString.NameValidating[Name] {
    val Anonymous = Name.unchecked("")
    override val name = "WorkflowJob.Name"

    protected def unchecked(string: String) = new Name(string)
  }

  /** To be used in Workflow with known WorkflowId. */
  implicit val jsonEncoder: ObjectEncoder[WorkflowJob] = workflowJob ⇒
    JsonObject.fromIterable(
      //(workflowJob.jobKey match {
      //  case JobKey.Named(_, name) ⇒ ("name" → name.asJson) :: Nil
      //  case _ ⇒ Nil
      //}) :::
      ("agentRefPath" → workflowJob.agentRefPath.asJson) ::
      ("executablePath" → workflowJob.executablePath.asJson) ::
      workflowJob.defaultArguments.nonEmpty.thenList("defaultArguments" → workflowJob.defaultArguments.asJson) :::
      (workflowJob.returnCodeMeaning != ReturnCodeMeaning.Default thenList ("returnCodeMeaning" → workflowJob.returnCodeMeaning.asJson)) :::
      ("taskLimit" → workflowJob.taskLimit.asJson) ::
      Nil)
  implicit val jsonDecoder: Decoder[WorkflowJob] = cursor ⇒
    for {
      //name ← cursor.get[Option[Name]]("name") map (_ getOrElse Name.Anonymous)
      executablePath ← cursor.get[ExecutablePath]("executablePath")
      agentRefPath ← cursor.get[AgentRefPath]("agentRefPath")
      arguments ← cursor.getOrElse[Map[String, String]]("defaultArguments")(Map.empty)
      rc ← cursor.getOrElse[ReturnCodeMeaning]("returnCodeMeaning")(ReturnCodeMeaning.Default)
      taskLimit ← cursor.get[Int]("taskLimit")
      job ← checked(agentRefPath, executablePath, arguments, rc, taskLimit).toDecoderResult
    } yield job
}
