package js7.data.execution.workflow.instructions

import js7.base.circeutils.CirceUtils._
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.data.agent.AgentRefPath
import js7.data.execution.workflow.context.OrderContext
import js7.data.execution.workflow.instructions.IfExecutorTest._
import js7.data.expression.Expression._
import js7.data.job.{ExecutablePath, ReturnCode}
import js7.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, If}
import js7.data.workflow.position.BranchId.{Else, Then}
import js7.data.workflow.position.{Position, WorkflowPosition}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class IfExecutorTest extends AnyFreeSpec {

  private lazy val context = new OrderContext {
    def idToOrder = Map(AOrder.id -> AOrder, BOrder.id -> BOrder)
    def childOrderEnded(order: Order[Order.State]) = throw new NotImplementedError
    def instruction(position: WorkflowPosition) = throw new NotImplementedError
    def idToWorkflow(id: WorkflowId) = throw new NotImplementedError
  }

  "JSON BranchId" - {
    "then" in {
      testJson(IfExecutor.nextPosition(context, AOrder, ifThenElse(BooleanConstant(true))).orThrow,
        json"""[ 7, "then", 0 ]""")
    }

    "else" in {
      testJson(IfExecutor.nextPosition(context, AOrder, ifThenElse(BooleanConstant(false))).orThrow,
        json"""[ 7, "else", 0 ]""")
    }
  }

  "If true" in {
    assert(InstructionExecutor.nextPosition(context, AOrder, ifThenElse(BooleanConstant(true))) ==
      Right(Some(Position(7) / Then % 0)))
  }

  "If false" in {
    assert(InstructionExecutor.nextPosition(context, AOrder, ifThenElse(BooleanConstant(false))) ==
      Right(Some(Position(7) / Else % 0)))
  }

  "If false, no else branch" in {
    assert(InstructionExecutor.nextPosition(context, AOrder, ifThen(BooleanConstant(false))) ==
      Right(Some(Position(8))))
  }

  "Naned value comparison" in {
    val expr = Equal(NamedValue.last("A"), StringConstant("AA"))
    assert(InstructionExecutor.nextPosition(context, AOrder, ifThenElse(expr)) == Right(Some(Position(7) / Then % 0)))
    assert(InstructionExecutor.nextPosition(context, BOrder, ifThenElse(expr)) == Right(Some(Position(7) / Else % 0)))
  }

  "Error in expression" in {
    val expr = Equal(ToNumber(StringConstant("X")), NumericConstant(1))
    assert(InstructionExecutor.nextPosition(context, AOrder, ifThenElse(expr)) == Left(Problem("Not a valid number: X")))
  }
}

object IfExecutorTest {
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") ~ "VERSION"
  private val AOrder = Order(OrderId("ORDER-A"), TestWorkflowId /: Position(7), Order.Processed,
    historicOutcomes = HistoricOutcome(Position(0), Outcome.Succeeded(ReturnCode(1), Map("A" -> "AA"))) :: Nil)
  private val BOrder = Order(OrderId("ORDER-B"), TestWorkflowId /: Position(7), Order.Processed,
    historicOutcomes = HistoricOutcome(Position(0), Outcome.Succeeded(ReturnCode(1), Map("A" -> "XX"))) :: Nil)
  private val ThenJob = Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/THEN")))
  private val ElseJob = Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/ELSE")))

  private def ifThenElse(booleanExpr: BooleanExpression) =
    If(booleanExpr, Workflow.of(ThenJob), Some(Workflow.of(ElseJob)))

  private def ifThen(booleanExpr: BooleanExpression) =
    If(booleanExpr, Workflow.of(ThenJob))
}
