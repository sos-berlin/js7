package js7.data.execution.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Checked.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.WallClock
import js7.data.agent.AgentPath
import js7.data.execution.workflow.instructions.IfExecutorTest.*
import js7.data.job.PathExecutable
import js7.data.order.OrderEvent.OrderMoved
import js7.data.order.{HistoricOutcome, Order, OrderId, OrderOutcome}
import js7.data.state.TestStateView
import js7.data.value.expression.Expression.*
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, If}
import js7.data.workflow.position.BranchId.{Else, Then}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class IfExecutorTest extends OurTestSuite:

  private val ifExecutor = new IfExecutor(new InstructionExecutorService(WallClock))
  private lazy val stateView = TestStateView.of(
    isAgent = false,
    orders = Some(Seq(AOrder, BOrder)),
    workflows = Some(Seq(Workflow.of(TestWorkflowId))))
  private lazy val executorService = new InstructionExecutorService(WallClock)

  "JSON BranchId" - {
    "then" in:
      testJson(
        ifExecutor.nextMove(ifThenElse(BooleanConstant(true)), AOrder, stateView).orThrow.get.to,
        json"""[ 7, "then", 0 ]""")

    "else" in:
      testJson(
        ifExecutor.nextMove(ifThenElse(BooleanConstant(false)), AOrder, stateView).orThrow.get.to,
        json"""[ 7, "else", 0 ]""")
  }

  "If true" in:
    assert(
      executorService.nextMove(ifThenElse(BooleanConstant(true)), AOrder, stateView).orThrow.get.to ==
        Position(7) / Then % 0)

  "If false" in:
    assert(
      executorService.nextMove(ifThenElse(BooleanConstant(false)), AOrder, stateView).orThrow.get.to ==
        Position(7) / Else % 0)

  "If false, no else branch" in:
    assert(executorService.nextMove(ifThen(BooleanConstant(false)), AOrder, stateView).orThrow.get.to ==
      Position(8))

  "Named value comparison" in:
    val expr = Equal(NamedValue("A"), StringConstant("AA"))
    assert(executorService.nextMove(ifThenElse(expr), AOrder, stateView) ==
      Right(Some(OrderMoved(Position(7) / Then % 0))))

    assert(executorService.nextMove(ifThenElse(expr), BOrder, stateView) ==
      Right(Some(OrderMoved(Position(7) / Else % 0))))

  "Error in expression" in:
    val expr = Equal(ToNumber(StringConstant("X")), NumericConstant(1))
    assert(executorService.nextMove(ifThenElse(expr), AOrder, stateView) == Left(Problem("Not a valid number: X")))


object IfExecutorTest:
  private val TestWorkflowId = WorkflowPath("WORKFLOW") ~ "VERSION"
  private val AOrder = Order(OrderId("ORDER-A"), TestWorkflowId /: Position(7), Order.Processed,
    historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Succeeded(NamedValues.rc(1) ++ Map("A" -> StringValue("AA"))))))
  private val BOrder = Order(OrderId("ORDER-B"), TestWorkflowId /: Position(7), Order.Processed,
    historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Succeeded(NamedValues.rc(1) ++ Map("A" -> StringValue("XX"))))))
  private val ThenJob = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("THEN")))
  private val ElseJob = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("ELSE")))

  private def ifThenElse(booleanExpr: BooleanExpr) =
    If(booleanExpr, Workflow.of(ThenJob), Some(Workflow.of(ElseJob)))

  private def ifThen(booleanExpr: BooleanExpr) =
    If(booleanExpr, Workflow.of(ThenJob))
