package js7.data.execution.workflow.instructions

import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.data.agent.{AgentPath, AtController}
import js7.data.controller.{ControllerEventColl, ControllerState}
import js7.data.job.ShellScriptExecutable
import js7.data.order.Order.Ready
import js7.data.order.{Order, OrderId}
import js7.data.value.expression.Expression.expr
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, Fork, Prompt}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}

final class EventInstructionExecutorTest extends OurTestSuite:

  "predictNextAgent" - {
    val orderId = OrderId("ORDER")
    val aAgentPath = AgentPath("A")
    val bAgentPath = AgentPath("B")

    val workflow = Workflow.of(WorkflowPath("WORKFLOW") ~ "VERSION",
      Fork(Vector(
        "A" -> Workflow.of:
          Execute(WorkflowJob(aAgentPath, ShellScriptExecutable("?"))),
        "B" -> Workflow.of:
          Execute(WorkflowJob(bAgentPath, ShellScriptExecutable("?"))))),
      Prompt(expr"'PROMPT'"))

    val coll = ControllerEventColl(
      ControllerState.forTest(workflows = Seq(workflow)),
      Timestamp.now)

    "predict the Agent" in:
      val checked = EventInstructionExecutor.predictNextAgent(
        Order(orderId / "A", workflow.id /: (Position(0) / "fork+A" % 0), Ready()),
        coll.forwardAs)
      assert(checked == Right(aAgentPath))

    "predict the Controller" in:
      locally:
        val checked = EventInstructionExecutor.predictNextAgent(
          Order(orderId, workflow.id /: Position(1) /*Prompt*/, Ready()),
          coll.forwardAs)
        assert(checked == Right(AtController))

      locally:
        val checked = EventInstructionExecutor.predictNextAgent(
          Order(orderId, workflow.id /: Position(2) /*End*/, Ready()),
          coll.forwardAs)
        assert(checked == Right(AtController))
  }
