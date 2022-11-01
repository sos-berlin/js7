package js7.data.execution.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.time.WallClock
import js7.data.agent.AgentPath
import js7.data.execution.workflow.instructions.TryExecutorTest.*
import js7.data.job.PathExecutable
import js7.data.order.OrderEvent.OrderMoved
import js7.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import js7.data.state.TestStateView
import js7.data.value.NamedValues
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, TryInstruction}
import js7.data.workflow.position.*
import js7.data.workflow.position.BranchId.try_
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class TryExecutorTest extends OurTestSuite
{
  private val tryExecutor = new TryExecutor(new InstructionExecutorService(WallClock))
  private lazy val stateView = TestStateView.of(
    isAgent = false,
    orders = Some(Seq(AOrder)))

  private lazy val executorService = new InstructionExecutorService(WallClock)

  "JSON" - {
    "try" in {
      testJson(tryExecutor.nextMove(tryInstruction, AOrder, stateView).orThrow.get.to,
        json"""[ 7, "try+0", 0 ]""")
    }

    "catch" in {
      testJson(Position(7) / BranchId.catch_(0) % 0,
        json"""[ 7, "catch+0", 0 ]""")
    }
  }

  "nextMove" in {
    assert(executorService.nextMove(tryInstruction, AOrder, stateView) ==
      Right(Some(OrderMoved(Position(7) / try_(0) % 0))))
  }

  "toEvents" in {
    assert(executorService.toEvents(tryInstruction, AOrder, stateView) ==
      Right(Seq(AOrder.id <-: OrderMoved(Position(7) / try_(0) % 0))))
  }
}

object TryExecutorTest {
  private val TestWorkflowId = WorkflowPath("WORKFLOW") ~ "VERSION"
  private val AOrder = Order(OrderId("ORDER-A"), TestWorkflowId /: Position(7), Order.Fresh,
    historicOutcomes = Vector(HistoricOutcome(Position(0), Outcome.Succeeded(NamedValues.rc(1)))))
  private val TryJob = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("THEN")))
  private val CatchJob = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("ELSE")))
  private val tryInstruction = TryInstruction(Workflow.of(TryJob), Workflow.of(CatchJob))
}
