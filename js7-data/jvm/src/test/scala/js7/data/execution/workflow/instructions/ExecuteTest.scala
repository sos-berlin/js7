package js7.data.execution.workflow.instructions

import js7.base.io.process.ReturnCode
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.time.WallClock
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.job.{RelativePathExecutable, ReturnCodeMeaning}
import js7.data.order.OrderEvent.{OrderActorEvent, OrderFailedIntermediate_, OrderMoved}
import js7.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import js7.data.state.StateView
import js7.data.value.{NamedValues, NumberValue, StringValue}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ExecuteTest extends AnyFreeSpec {

  private val executable = RelativePathExecutable("EXECUTABLE", returnCodeMeaning = ReturnCodeMeaning.Success.of(0, 3, 9))
  private val executeAnonymous = Execute(WorkflowJob(AgentPath("AGENT"), executable))
  private val orderId = OrderId("ORDER")
  private val executeExecutor = new ExecuteExecutor(new InstructionExecutorService(WallClock))

  private val stateView = StateView.forTest(isAgent = false)

  "toOutcome" in {
    val namedValues = Map("a" -> StringValue("A"))
    assert(executable.toOutcome(namedValues, ReturnCode(0)) == Outcome.Succeeded(namedValues + ("returnCode" -> NumberValue(0))))
    assert(executable.toOutcome(namedValues, ReturnCode(1)) == Outcome.Failed   (namedValues + ("returnCode" -> NumberValue(1))))
    assert(executable.toOutcome(namedValues, ReturnCode(3)) == Outcome.Succeeded(namedValues + ("returnCode" -> NumberValue(3))))
  }

  "toEvents" in {
    assert(toEvents(Outcome.Succeeded(NamedValues.rc(0))) == Seq(orderId <-: OrderMoved(Position(1) / "A" % 21)))
    assert(toEvents(Outcome.Succeeded(NamedValues.rc(1))) == Seq(orderId <-: OrderMoved(Position(1) / "A" % 21)))
    assert(toEvents(Outcome.Failed(NamedValues.rc(1))) == Seq(orderId <-: OrderFailedIntermediate_()))
    assert(toEvents(Outcome.Disrupted(Problem("DISRUPTION"))) == Seq(orderId <-: OrderFailedIntermediate_()))
  }

  private def toEvents(outcome: Outcome): Seq[KeyedEvent[OrderActorEvent]] = {
    val order = Order(orderId, (WorkflowPath("WORKFLOW") ~ "VERSION" ) /: (Position(1) / "A" % 20), Order.Processed,
      historicOutcomes = Vector(HistoricOutcome(Position(1) / "B" % 20, outcome)))
    executeExecutor.toEvents(executeAnonymous, order, stateView).orThrow
  }
}
