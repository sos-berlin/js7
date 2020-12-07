package js7.data.execution.workflow.instructions

import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.data.agent.AgentName
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.context.OrderContext
import js7.data.job.{RelativeExecutablePath, ReturnCode}
import js7.data.order.OrderEvent.{OrderActorEvent, OrderFailedIntermediate_, OrderMoved}
import js7.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import js7.data.value.{NamedValues, NumericValue, StringValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ReturnCodeMeaning}
import js7.data.workflow.position.Position
import js7.data.workflow.{WorkflowId, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ExecuteTest extends AnyFreeSpec {

  private val executeAnonymous = Execute(WorkflowJob(AgentName("AGENT"), RelativeExecutablePath("EXECUTABLE"),
    returnCodeMeaning = ReturnCodeMeaning.Success.of(0, 3, 9)))
  private val orderId = OrderId("ORDER")

  private val orderContext = new OrderContext {
    def childOrderEnded(order: Order[Order.State]) = throw new NotImplementedError
    def idToOrder = throw new NotImplementedError
    def idToWorkflow(id: WorkflowId) = throw new NotImplementedError
    val nameToLockState = _ => Left(Problem("nameToLockState is not implemented here"))
  }

  "toOutcome" in {
    val namedValues = Map("a" -> StringValue("A"))
    assert(executeAnonymous.job.toOutcome(namedValues, ReturnCode(0)) == Outcome.Succeeded(namedValues + ("returnCode" -> NumericValue(0))))
    assert(executeAnonymous.job.toOutcome(namedValues, ReturnCode(1)) == Outcome.Failed   (namedValues + ("returnCode" -> NumericValue(1))))
    assert(executeAnonymous.job.toOutcome(namedValues, ReturnCode(3)) == Outcome.Succeeded(namedValues + ("returnCode" -> NumericValue(3))))
  }

  "toEvents" in {
    assert(toEvents(Outcome.Succeeded(NamedValues.rc(0))) == Seq(orderId <-: OrderMoved(Position(1) / "A" % 21)))
    assert(toEvents(Outcome.Succeeded(NamedValues.rc(1))) == Seq(orderId <-: OrderMoved(Position(1) / "A" % 21)))
    assert(toEvents(Outcome.Failed(NamedValues.rc(1))) == Seq(orderId <-: OrderFailedIntermediate_()))
    assert(toEvents(Outcome.Disrupted(Problem("DISRUPTION"))) == Seq(orderId <-: OrderFailedIntermediate_()))
  }

  private def toEvents(outcome: Outcome): Seq[KeyedEvent[OrderActorEvent]] = {
    val order = Order(orderId, (WorkflowPath("/WORKFLOW") ~ "VERSION" ) /: (Position(1) / "A" % 20), Order.Processed,
      historicOutcomes = HistoricOutcome(Position(1) / "B" % 20, outcome) :: Nil)
    ExecuteExecutor.toEvents(executeAnonymous, order, orderContext).orThrow
  }
}
