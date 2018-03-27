package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.core.workflow.instructions.IfExecutorTest._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.{JobPath, ReturnCode}
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression._
import com.sos.jobscheduler.data.workflow.instructions.{If, Job}
import com.sos.jobscheduler.data.workflow.{OrderContext, Position, Workflow, WorkflowPath, WorkflowPosition}
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

  "If true" in {
    assert(InstructionExecutor.nextPosition(context, AOrder, ifThenElse(BooleanConstant(true))) ==
      Some(Position(7, 0/*then*/, 0)))
  }

  "If false" in {
    assert(InstructionExecutor.nextPosition(context, AOrder, ifThenElse(BooleanConstant(false))) ==
      Some(Position(7, 1/*else*/, 0)))
  }

  "If false, no else branch" in {
    assert(InstructionExecutor.nextPosition(context, AOrder, ifThen(BooleanConstant(false))) ==
      Some(Position(8)))
  }

  "Variable comparison" in {
    val expr = Equal(Variable(StringConstant("A")), StringConstant("AA"))
    assert(InstructionExecutor.nextPosition(context, AOrder, ifThenElse(expr)) == Some(Position(7, 0/*then*/, 0)))
    assert(InstructionExecutor.nextPosition(context, BOrder, ifThenElse(expr)) == Some(Position(7, 1/*else*/, 0)))
  }

  "Error in expression" in {
    val expr = Equal(ToNumber(StringConstant("X")), NumericConstant(1))
    assert(InstructionExecutor.nextPosition(context, AOrder, ifThenElse(expr)) == None) // TODO == Invalid(Problem("Not a valid number: X"))
  }
}

object IfExecutorTest {
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") % "VERSION"
  val AOrderId = OrderId("OFFERED")
  val AOrder = Order(OrderId("ORDER-A"), TestWorkflowId /: Position(7), Order.Processed(Outcome.Succeeded(ReturnCode(1))),
    payload = Payload(Map("A" → "AA")))
  val BOrder = Order(OrderId("ORDER-B"), TestWorkflowId /: Position(7), Order.Processed(Outcome.Succeeded(ReturnCode(1))),
    payload = Payload(Map("A" → "XX")))
  val ThenJob = Job(JobPath("/THEN"), AgentPath("/AGENT"))
  val ElseJob = Job(JobPath("/ELSE"), AgentPath("/AGENT"))

  private def ifThenElse(booleanExpr: BooleanExpression) =
    If(booleanExpr, Workflow.of(ThenJob), Some(Workflow.of(ElseJob)))

  private def ifThen(booleanExpr: BooleanExpression) =
    If(booleanExpr, Workflow.of(ThenJob))
}
