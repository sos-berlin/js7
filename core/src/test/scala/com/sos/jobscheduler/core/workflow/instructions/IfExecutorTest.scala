package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.core.workflow.instructions.IfExecutorTest._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.instructions.If.{Else, Then}
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression._
import com.sos.jobscheduler.data.workflow.instructions.{Execute, If}
import com.sos.jobscheduler.data.workflow.position.{Position, WorkflowPosition}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class IfExecutorTest extends FreeSpec {

  private lazy val context = new OrderContext {
    def idToOrder = Map(AOrder.id → AOrder, BOrder.id → BOrder)
    def childOrderEnded(order: Order[Order.State]) = throw new NotImplementedError
    def instruction(position: WorkflowPosition) = throw new NotImplementedError
  }

  "JSON" - {
    "then" in {
      testJson(IfExecutor.nextPosition(context, AOrder, ifThenElse(BooleanConstant(true))),
        json"""[ 7, "then", 0 ]""")
    }

    "else" in {
      testJson(IfExecutor.nextPosition(context, AOrder, ifThenElse(BooleanConstant(false))),
        json"""[ 7, "else", 0 ]""")
    }
  }

  "If true" in {
    assert(InstructionExecutor.nextPosition(context, AOrder, ifThenElse(BooleanConstant(true))) ==
      Some(Position(7) / Then % 0))
  }

  "If false" in {
    assert(InstructionExecutor.nextPosition(context, AOrder, ifThenElse(BooleanConstant(false))) ==
      Some(Position(7) / Else % 0))
  }

  "If false, no else branch" in {
    assert(InstructionExecutor.nextPosition(context, AOrder, ifThen(BooleanConstant(false))) ==
      Some(Position(8)))
  }

  "Variable comparison" in {
    val expr = Equal(Variable(StringConstant("A")), StringConstant("AA"))
    assert(InstructionExecutor.nextPosition(context, AOrder, ifThenElse(expr)) == Some(Position(7) / Then % 0))
    assert(InstructionExecutor.nextPosition(context, BOrder, ifThenElse(expr)) == Some(Position(7) / Else % 0))
  }

  "Error in expression" in {
    val expr = Equal(ToNumber(StringConstant("X")), NumericConstant(1))
    assert(InstructionExecutor.nextPosition(context, AOrder, ifThenElse(expr)) == None) // TODO == Invalid(Problem("Not a valid number: X"))
  }
}

object IfExecutorTest {
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") % "VERSION"
  private val AOrder = Order(OrderId("ORDER-A"), TestWorkflowId /: Position(7), Order.Processed, outcome = Outcome.Succeeded(ReturnCode(1)),
    payload = Payload(Map("A" → "AA")))
  private val BOrder = Order(OrderId("ORDER-B"), TestWorkflowId /: Position(7), Order.Processed, outcome = Outcome.Succeeded(ReturnCode(1)),
    payload = Payload(Map("A" → "XX")))
  private val ThenJob = Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/THEN")))
  private val ElseJob = Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/ELSE")))

  private def ifThenElse(booleanExpr: BooleanExpression) =
    If(booleanExpr, Workflow.of(ThenJob), Some(Workflow.of(ElseJob)))

  private def ifThen(booleanExpr: BooleanExpression) =
    If(booleanExpr, Workflow.of(ThenJob))
}
