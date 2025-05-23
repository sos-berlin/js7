package js7.data.execution.workflow.instructions

import js7.base.io.process.ReturnCode
import js7.base.problem.Checked.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.WallClock
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.job.{InternalExecutable, RelativePathExecutable, ReturnCodeMeaning}
import js7.data.order.OrderEvent.{OrderActorEvent, OrderFailedIntermediate_, OrderMoved}
import js7.data.order.{HistoricOutcome, Order, OrderId, OrderOutcome}
import js7.data.state.ControllerTestStateView
import js7.data.value.{NamedValues, NumberValue, StringValue}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}

/**
  * @author Joacim Zschimmer
  */
final class ExecuteTest extends OurTestSuite:
  private val executable = RelativePathExecutable("EXECUTABLE", returnCodeMeaning = ReturnCodeMeaning.Success.of(0, 3, 9))
  private val executeAnonymous = Execute(WorkflowJob(AgentPath("AGENT"), executable))
  private val orderId = OrderId("ORDER")
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "VERSION",
    Seq(Execute(WorkflowJob(AgentPath("AGENT"), InternalExecutable("?")))))
  private val executeExecutor = new ExecuteExecutor(new InstructionExecutorService(WallClock))

  private val stateView = ControllerTestStateView.of(
    workflows = Some(Seq(workflow)))

  "toOutcome" in:
    val namedValues = Map("a" -> StringValue("A"))
    assert(executable.toOutcome(namedValues, ReturnCode(0)) == OrderOutcome.Succeeded(namedValues + ("returnCode" -> NumberValue(0))))
    assert(executable.toOutcome(namedValues, ReturnCode(1)) == OrderOutcome.Failed   (namedValues + ("returnCode" -> NumberValue(1))))
    assert(executable.toOutcome(namedValues, ReturnCode(3)) == OrderOutcome.Succeeded(namedValues + ("returnCode" -> NumberValue(3))))

  "toEvents" in:
    assert(toEvents(OrderOutcome.Succeeded(NamedValues.rc(0))) == Seq(orderId <-: OrderMoved(Position(1))))
    assert(toEvents(OrderOutcome.Succeeded(NamedValues.rc(1))) == Seq(orderId <-: OrderMoved(Position(1))))
    assert(toEvents(OrderOutcome.Failed(NamedValues.rc(1))) == Seq(orderId <-: OrderFailedIntermediate_()))
    assert(toEvents(OrderOutcome.Disrupted(Problem("DISRUPTION"))) == Seq(orderId <-: OrderFailedIntermediate_()))

  private def toEvents(outcome: OrderOutcome): Seq[KeyedEvent[OrderActorEvent]] =
    val order = Order(orderId, workflow.id /: (Position(0)), Order.Processed,
      historicOutcomes = Vector(HistoricOutcome(Position(0), outcome)))
    executeExecutor.toEvents(executeAnonymous, order, stateView).orThrow
