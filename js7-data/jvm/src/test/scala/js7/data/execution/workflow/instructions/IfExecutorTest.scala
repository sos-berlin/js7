package js7.data.execution.workflow.instructions

import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Checked.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.data.agent.AgentPath
import js7.data.controller.ControllerState
import js7.data.execution.workflow.instructions.IfExecutorTest.*
import js7.data.job.PathExecutable
import js7.data.order.OrderEvent.OrderMoved
import js7.data.order.{HistoricOutcome, Order, OrderId, OrderOutcome}
import js7.data.value.expression.Expression.*
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{EmptyInstruction, Execute, If}
import js7.data.workflow.position.BranchId.{Else, Then}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class IfExecutorTest extends OurTestSuite:

  private lazy val engineState = ControllerState.forTest(
    orders = Seq(AOrder, BOrder),
    workflows = Seq(Workflow.of(TestWorkflowId)))

  "JSON BranchId" - {
    "then" in:
      testJson(
        IfExecutor.nextMove(ifThenElse(BooleanConstant(true)), AOrder, engineState, Timestamp.now)
          .orThrow.get.to,
        json"""[ 1, "then", 0 ]""")

    "else" in:
      testJson(
        IfExecutor.nextMove(ifThenElse(BooleanConstant(false)), AOrder, engineState, Timestamp.now)
          .orThrow.get.to,
        json"""[ 1, "else", 0 ]""")
  }

  "If true" in:
    assert:
      nextMove(ifThenElse(BooleanConstant(true)), AOrder) ==
        Right(Some(OrderMoved(Position(1) / Then % 0)))

  "If false" in:
    assert:
      nextMove(ifThenElse(BooleanConstant(false)), AOrder) ==
        Right(Some(OrderMoved(Position(1) / Else % 0)))

  "If false, no else branch" in:
    assert:
      nextMove(ifThen(BooleanConstant(false)), AOrder) ==
        Right(Some(OrderMoved(Position(2))))

  "Named value comparison" in:
    val expr = Equal(NamedValue("A"), StringConstant("AA"))
    assert:
      nextMove(ifThenElse(expr), AOrder) ==
        Right(Some(OrderMoved(Position(1) / Then % 0)))

    assert:
      nextMove(ifThenElse(expr), BOrder) ==
        Right(Some(OrderMoved(Position(1) / Else % 0)))

  "Error in expression" in:
    val expr = Equal(ToNumber(StringConstant("X")), NumericConstant(1))
    assert(nextMove(ifThenElse(expr), AOrder) == Left(Problem("Not a valid number: X")))

  private def nextMove(instr: If, order: Order[Order.State]) =
    assert(order.position == Position(1))
    val engineState = ControllerState.forTest(
      orders = Seq(order),
      workflows = Seq(Workflow.of(
        TestWorkflowId,
        EmptyInstruction(),
        instr)))
    InstructionExecutor.nextMove(order, engineState, Timestamp.now)

  "JS-2134 else if" in:
    val instr =
      If(expr("false")).Then:
        Workflow.of(ThenJob)
      .elseIf(expr("false")).Then:
        Workflow.of(Then2Job)
      .elseIf(expr("true")).Then:
        Workflow.of(Then3Job)
      .Else:
        Workflow.of(ElseJob)

    testJson(
      IfExecutor.nextMove(instr, AOrder, engineState, Timestamp.now).orThrow.get.to,
      json"""[ 1, "then+3", 0 ]""")


object IfExecutorTest:
  private val TestWorkflowId = WorkflowPath("WORKFLOW") ~ "VERSION"
  private val AOrder = Order(OrderId("ORDER-A"), TestWorkflowId /: Position(1), Order.Processed,
    historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Succeeded(NamedValues.rc(1) ++ Map("A" -> StringValue("AA"))))))
  private val BOrder = Order(OrderId("ORDER-B"), TestWorkflowId /: Position(1), Order.Processed,
    historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Succeeded(NamedValues.rc(1) ++ Map("A" -> StringValue("XX"))))))
  private val ThenJob = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("THEN")))
  private val Then2Job = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("THEN-2")))
  private val Then3Job = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("THEN-2")))
  private val ElseJob = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("ELSE")))

  private def ifThenElse(booleanExpr: BooleanExpr) =
    If(booleanExpr).Then:
      Workflow.of(ThenJob)
    .Else:
      Workflow.of(ElseJob)

  private def ifThen(booleanExpr: BooleanExpr) =
    If(booleanExpr)(Workflow.of(ThenJob))
