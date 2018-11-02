package com.sos.jobscheduler.data.workflow.instructions.executable

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.circeutils.CirceUtils.CirceUtilsChecked
import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.OrderProcessed
import com.sos.jobscheduler.data.order.Outcome
import com.sos.jobscheduler.data.workflow.instructions.ReturnCodeMeaning
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowJob private(
  agentPath: AgentPath,
  executablePath: ExecutablePath,
  defaultArguments: Map[String, String],
  returnCodeMeaning: ReturnCodeMeaning,
  taskLimit: Int)
{
  def toOrderProcessed(variablesDiff: MapDiff[String, String], returnCode: ReturnCode) =
    OrderProcessed(variablesDiff, Outcome.Undisrupted(returnCode, success = returnCodeMeaning.isSuccess(returnCode)))

  def isExecutableOnAgent(agentPath: AgentPath): Boolean =
    this.agentPath == agentPath

  override def toString = s"XX on ${agentPath.string}" + (
    returnCodeMeaning match {
      case ReturnCodeMeaning.Default ⇒ ""
      case ReturnCodeMeaning.Success(returnCodes) ⇒ s" successReturnCodes=(${returnCodes.map(_.number) mkString ", "})"
      case ReturnCodeMeaning.Failure(returnCodes) ⇒ s" failureReturnCodes=(${returnCodes.map(_.number) mkString ", "})"
    })
}

object WorkflowJob
{
  def apply(
    agentPath: AgentPath,
    executablePath: ExecutablePath,
    defaultArguments: Map[String, String] = Map.empty,
    returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
    taskLimit: Int = 1): WorkflowJob
  = checked(agentPath, executablePath, defaultArguments, returnCodeMeaning, taskLimit).orThrow

  def checked(
    agentPath: AgentPath,
    executablePath: ExecutablePath,
    defaultArguments: Map[String, String] = Map.empty,
    returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default,
    taskLimit: Int = 1): Checked[WorkflowJob]
  =
    if (agentPath.isAnonymous)
      Problem.fromEager("Anonymous Agent in Job?")
    else
      Valid(new WorkflowJob(agentPath, executablePath, defaultArguments, returnCodeMeaning, taskLimit))

  final case class Name(string: String) extends GenericString
  object Name extends GenericString.NameValidating[Name] {
    val Anonymous = Name("")
    override val name = "WorkflowJob.Name"
  }

  /** To be used in Workflow with known WorkflowId. */
  implicit val jsonEncoder: ObjectEncoder[WorkflowJob] = workflowJob ⇒
    JsonObject.fromIterable(
      //(workflowJob.jobKey match {
      //  case JobKey.Named(_, name) ⇒ ("name" → name.asJson) :: Nil
      //  case _ ⇒ Nil
      //}) :::
      ("agentPath" → workflowJob.agentPath.asJson) ::
      ("executablePath" → workflowJob.executablePath.asJson) ::
      workflowJob.defaultArguments.nonEmpty.list("defaultArguments" → workflowJob.defaultArguments.asJson) :::
      (workflowJob.returnCodeMeaning != ReturnCodeMeaning.Default list ("returnCodeMeaning" → workflowJob.returnCodeMeaning.asJson)) :::
      ("taskLimit" → workflowJob.taskLimit.asJson) ::
      Nil)
  implicit val jsonDecoder: Decoder[WorkflowJob] = cursor ⇒
    for {
      //name ← cursor.get[Option[Name]]("name") map (_ getOrElse Name.Anonymous)
      executablePath ← cursor.get[ExecutablePath]("executablePath")
      agentPath ← cursor.get[AgentPath]("agentPath")
      arguments ← cursor.getOrElse[Map[String, String]]("defaultArguments")(Map.empty)
      rc ← cursor.getOrElse[ReturnCodeMeaning]("returnCodeMeaning")(ReturnCodeMeaning.Default)
      taskLimit ← cursor.get[Int]("taskLimit")
      job ← checked(agentPath, executablePath, arguments, rc, taskLimit).toDecoderResult
    } yield job
}
