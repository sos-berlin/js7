package js7.data.execution.workflow.instructions

import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked._
import js7.data.agent.AgentPath
import js7.data.execution.workflow.context.StateView
import js7.data.execution.workflow.instructions.TryExecutorTest._
import js7.data.job.PathExecutable
import js7.data.order.OrderEvent.OrderMoved
import js7.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import js7.data.value.NamedValues
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, TryInstruction}
import js7.data.workflow.position.BranchId.try_
import js7.data.workflow.position._
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TryExecutorTest extends AnyFreeSpec
{
  private lazy val stateView = StateView.forTest(
    isAgent = false,
    idToOrder = Map(AOrder.id -> AOrder))

  "JSON" - {
    "try" in {
      testJson(TryExecutor.nextPosition(tryInstruction, AOrder, stateView).orThrow,
        json"""[ 7, "try+0", 0 ]""")
    }

    "catch" in {
      testJson(Position(7) / BranchId.catch_(0) % 0,
        json"""[ 7, "catch+0", 0 ]""")
    }
  }

  "nextPosition" in {
    assert(InstructionExecutor.nextPosition(tryInstruction, AOrder, stateView) ==
      Right(Some(Position(7) / try_(0) % 0)))
  }

  "toEvents" in {
    assert(InstructionExecutor.toEvents(tryInstruction, AOrder, stateView) ==
      Right(Seq(AOrder.id <-: OrderMoved(Position(7) / try_(0) % 0))))
  }
}

object TryExecutorTest {
  private val TestWorkflowId = WorkflowPath("WORKFLOW") ~ "VERSION"
  private val AOrder = Order(OrderId("ORDER-A"), TestWorkflowId /: Position(7), Order.Fresh,
    historicOutcomes = HistoricOutcome(Position(0), Outcome.Succeeded(NamedValues.rc(1))) :: Nil)
  private val TryJob = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("THEN")))
  private val CatchJob = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("ELSE")))
  private val tryInstruction = TryInstruction(Workflow.of(TryJob), Workflow.of(CatchJob))
}
