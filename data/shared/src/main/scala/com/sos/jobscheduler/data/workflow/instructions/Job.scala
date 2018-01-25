package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent.{OrderMoved, OrderProcessed, OrderStopped}
import com.sos.jobscheduler.data.order.Outcome.Disrupted.JobSchedulerRestarted
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.{AgentJobPath, EventInstruction, JobPath, OrderContext}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
final case class Job(job: AgentJobPath, returnCodeMeaning: ReturnCodeMeaning = ReturnCodeMeaning.Default) extends EventInstruction
{
  def toOrderProcessed(variablesDiff: MapDiff[String, String], returnCode: ReturnCode) =
    OrderProcessed(variablesDiff, Outcome.Undisrupted(returnCode, success = returnCodeMeaning.isSuccess(returnCode)))

  def toEvent(order: Order[Order.State], context: OrderContext) =
    // Order.Ready: Job start has to be done by the caller
    for (order ← order.ifState[Order.Processed]) yield
      order.id <-: (
        order.state.outcome match {
          case Outcome.Disrupted(JobSchedulerRestarted) ⇒
            OrderMoved(order.position)  // Repeat

          case _: Outcome.Succeeded ⇒
            OrderMoved(order.position.increment)

          case failed: Outcome.NotSucceeded ⇒
            OrderStopped(failed)
        })


  def agentPath = job.agentPath

  def jobPath = job.jobPath

  def isExecutableOnAgent(agentPath: AgentPath): Boolean =
    job.agentPath == agentPath

  override def toString = s"job ${jobPath.string} on ${agentPath.string}" + (
    returnCodeMeaning match {
      case ReturnCodeMeaning.Default ⇒ ""
      case ReturnCodeMeaning.Success(returnCodes) ⇒ s" successReturnCodes=(${returnCodes.map(_.number) mkString ", "})"
      case ReturnCodeMeaning.Failure(returnCodes) ⇒ s" failureReturnCodes=(${returnCodes.map(_.number) mkString ", "})"
    }
  )
}

object Job {
  def apply(path: JobPath, agentPath: AgentPath) =
    new Job(AgentJobPath(agentPath, path))

  def apply(path: JobPath, agentPath: AgentPath, returnCodeMeaning: ReturnCodeMeaning) =
    new Job(AgentJobPath(agentPath, path), returnCodeMeaning)

  implicit val jsonEncoder: ObjectEncoder[Job] = job ⇒
    JsonObject.fromIterable(
      ("job" → job.job.asJson) ::
        (job.returnCodeMeaning != ReturnCodeMeaning.Default list ("returnCodeMeaning" → job.returnCodeMeaning.asJson)))
  implicit val jsonDecoder: Decoder[Job] = cursor ⇒
    for {
      job ← cursor.get[AgentJobPath]("job")
      rc ← cursor.getOrElse[ReturnCodeMeaning]("returnCodeMeaning")(ReturnCodeMeaning.Default)
    } yield Job(job, rc)
}
