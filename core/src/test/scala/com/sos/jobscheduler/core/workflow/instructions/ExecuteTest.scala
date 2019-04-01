package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderFailed, OrderMoved, OrderProcessed}
import com.sos.jobscheduler.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{Execute, ReturnCodeMeaning}
import com.sos.jobscheduler.data.workflow.position.{Position, WorkflowPosition}
import com.sos.jobscheduler.data.workflow.{WorkflowId, WorkflowPath}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ExecuteTest extends FreeSpec {

  private val executeAnonymous = Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/EXECUTABLE"),
    returnCodeMeaning = ReturnCodeMeaning.Success.of(0, 3, 9)))
  private val orderId = OrderId("ORDER")

  private val orderContext = new OrderContext {
    def childOrderEnded(order: Order[Order.State]) = throw new NotImplementedError
    def idToOrder = throw new NotImplementedError
    def instruction(workflowPosition: WorkflowPosition) = throw new NotImplementedError
    def idToWorkflow(id: WorkflowId) = throw new NotImplementedError
  }

  "toOrderProcessed" in {
    val keyValues = Map("a" -> "A")
    assert(executeAnonymous.job.toOrderProcessed(ReturnCode(0), keyValues) == OrderProcessed(Outcome.Succeeded(keyValues)))
    assert(executeAnonymous.job.toOrderProcessed(ReturnCode(1), keyValues) == OrderProcessed(Outcome.Failed(ReturnCode(1), keyValues)))
    assert(executeAnonymous.job.toOrderProcessed(ReturnCode(3), keyValues) == OrderProcessed(Outcome.Succeeded(ReturnCode(3), keyValues)))
  }

  "toEvent" in {
    assert(toEvent(Outcome.Succeeded(ReturnCode(0))) == Some(orderId <-: OrderMoved(Position(1) / 2 % 4)))
    assert(toEvent(Outcome.Succeeded(ReturnCode(1))) == Some(orderId <-: OrderMoved(Position(1) / 2 % 4)))
    assert(toEvent(Outcome.Failed(ReturnCode(1))) == Some(orderId <-: OrderFailed(Outcome.Failed(ReturnCode(1)))))
    assert(toEvent(Outcome.Disrupted(Problem("DISRUPTION"))) == Some(orderId <-: OrderFailed(Outcome.Disrupted(Problem("DISRUPTION")))))
  }

  private def toEvent(outcome: Outcome): Option[KeyedEvent[OrderActorEvent]] = {
    val order = Order(orderId, (WorkflowPath("/WORKFLOW") ~ "VERSION" ) /: (Position(1) / 2 % 3), Order.Processed,
      historicOutcomes = HistoricOutcome(Position(1) / 2 % 2, outcome) :: Nil)
    ExecuteExecutor.toEvent(orderContext, order, executeAnonymous).orThrow
  }
}
