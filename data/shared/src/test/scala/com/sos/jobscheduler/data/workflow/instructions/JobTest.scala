package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderMoved, OrderProcessed, OrderStopped}
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.{JobPath, OrderContext, Position, WorkflowPath, WorkflowPosition}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JobTest extends FreeSpec {

  private val job = Job(JobPath("/JOB"), AgentPath("/AGENT"), ReturnCodeMeaning.Success.of(0, 3, 9))
  private val orderId = OrderId("ORDER")

  private val orderContext = new OrderContext {
    def childOrderEnded(order: Order[Order.State]) = throw new NotImplementedError
    def idToOrder = throw new NotImplementedError
    def instruction(workflowPosition: WorkflowPosition) = throw new NotImplementedError
  }

  "toOrderProcessed" in {
    val variablesDiff = MapDiff(Map("a" → "A"))
    assert(job.toOrderProcessed(variablesDiff, ReturnCode(0)) == OrderProcessed(variablesDiff, Outcome.Succeeded(ReturnCode(0))))
    assert(job.toOrderProcessed(variablesDiff, ReturnCode(1)) == OrderProcessed(variablesDiff, Outcome.Failed(ReturnCode(1))))
    assert(job.toOrderProcessed(variablesDiff, ReturnCode(3)) == OrderProcessed(variablesDiff, Outcome.Succeeded(ReturnCode(3))))
  }

  "toEvent" in {
    assert(toEvent(Outcome.Succeeded(ReturnCode(0))) == Some(orderId <-: OrderMoved(Position(1, 2, 4))))
    assert(toEvent(Outcome.Succeeded(ReturnCode(1))) == Some(orderId <-: OrderMoved(Position(1, 2, 4))))
    assert(toEvent(Outcome.Failed(ReturnCode(1))) == Some(orderId <-: OrderStopped(Outcome.Failed(ReturnCode(1)))))
    assert(toEvent(Outcome.Disrupted("DISRUPTION")) == Some(orderId <-: OrderStopped(Outcome.Disrupted("DISRUPTION"))))
  }

  private def toEvent(outcome: Outcome): Option[KeyedEvent[OrderActorEvent]] = {
    val order = Order(orderId, WorkflowPath("/WORKFLOW") /: Position(1, 2, 3), Order.Processed(outcome))
    job.toEvent(order, orderContext)
  }
}
