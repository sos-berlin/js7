package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.{JobPath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.OrderProcessed
import com.sos.jobscheduler.data.order.Outcome
import com.sos.jobscheduler.data.workflow.Instruction
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
final case class Job(jobPath: JobPath, agentPath: AgentPath, returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default) extends Instruction
{
  def toOrderProcessed(variablesDiff: MapDiff[String, String], returnCode: ReturnCode) =
    OrderProcessed(variablesDiff, Outcome.Undisrupted(returnCode, success = returnCodeMeaning.isSuccess(returnCode)))

  def isExecutableOnAgent(agentPath: AgentPath): Boolean =
    this.agentPath == agentPath

  override def toString = s"job ${jobPath.string} on ${agentPath.string}" + (
    returnCodeMeaning match {
      case ReturnCodeMeaning.Default ⇒ ""
      case ReturnCodeMeaning.Success(returnCodes) ⇒ s" successReturnCodes=(${returnCodes.map(_.number) mkString ", "})"
      case ReturnCodeMeaning.Failure(returnCodes) ⇒ s" failureReturnCodes=(${returnCodes.map(_.number) mkString ", "})"
    }
  )
}

object Job {
  implicit val jsonEncoder: ObjectEncoder[Job] = job ⇒
    JsonObject.fromIterable(
      ("jobPath" → job.jobPath.asJson) ::
      ("agentPath" → job.agentPath.asJson) ::
      (job.returnCodeMeaning != ReturnCodeMeaning.Default list ("returnCodeMeaning" → job.returnCodeMeaning.asJson)))
  implicit val jsonDecoder: Decoder[Job] = cursor ⇒
    for {
      jobPath ← cursor.get[JobPath]("jobPath")
      agentPath ← cursor.get[AgentPath]("agentPath")
      rc ← cursor.getOrElse[ReturnCodeMeaning]("returnCodeMeaning")(ReturnCodeMeaning.Default)
    } yield Job(jobPath, agentPath, rc)
}
