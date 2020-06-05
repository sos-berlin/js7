package js7.data.execution.workflow.instructions

import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.data.agent.AgentRefPath
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.context.OrderContext
import js7.data.job.{ExecutablePath, ReturnCode}
import js7.data.order.OrderEvent.{OrderActorEvent, OrderFailedCatchable, OrderMoved, OrderProcessed}
import js7.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ReturnCodeMeaning}
import js7.data.workflow.position.{Position, WorkflowPosition}
import js7.data.workflow.{WorkflowId, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ExecuteTest extends AnyFreeSpec {

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
    assert(toEvent(Outcome.Failed(ReturnCode(1))) == Some(orderId <-: OrderFailedCatchable(Outcome.Failed(ReturnCode(1)))))
    assert(toEvent(Outcome.Disrupted(Problem("DISRUPTION"))) == Some(orderId <-: OrderFailedCatchable(Outcome.Disrupted(Problem("DISRUPTION")))))
  }

  private def toEvent(outcome: Outcome): Option[KeyedEvent[OrderActorEvent]] = {
    val order = Order(orderId, (WorkflowPath("/WORKFLOW") ~ "VERSION" ) /: (Position(1) / 2 % 3), Order.Processed,
      historicOutcomes = HistoricOutcome(Position(1) / 2 % 2, outcome) :: Nil)
    ExecuteExecutor.toEvent(orderContext, order, executeAnonymous).orThrow
  }
}
