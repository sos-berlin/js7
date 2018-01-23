package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.OrderEvent.OrderMoved
import com.sos.jobscheduler.data.order.Outcome.Bad.AgentRestarted
import com.sos.jobscheduler.data.order.{Order, Outcome}
import com.sos.jobscheduler.data.workflow.{AgentJobPath, EventInstruction, OrderContext}
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class Job(job: AgentJobPath) extends EventInstruction
{
  def toEvent(order: Order[Order.State], context: OrderContext) =
    // Order.Ready: Job start has to be done by the caller
    for {
      order ← order.ifState[Order.Processed.type]
      event ←
        if (order.outcome == Outcome.Bad(AgentRestarted))
          Some(order.id <-: OrderMoved(order.position))  // Repeat
        else
          for (to ← context.nextPosition(order)) yield
            order.id <-: OrderMoved(to)
    } yield event

  def agentPath = job.agentPath

  def jobPath = job.jobPath

  def isExecutableOnAgent(agentPath: AgentPath): Boolean =
    job.agentPath == agentPath

  override def toString = s"job ${jobPath.string} on ${agentPath.string}"
}
