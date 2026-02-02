package js7.data.execution.workflow.instructions

import js7.base.test.OurTestSuite
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.controller.{ControllerEventColl, ControllerState}
import js7.data.event.Event
import js7.data.execution.workflow.OrderEventSource
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderForked, OrderStarted}
import js7.data.order.OrderId
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, Fork}
import js7.data.workflow.{Workflow, WorkflowPath}

final class ForkInstructionExecutorTest extends OurTestSuite:

  "predictControllerOrAgent" - {
    val orderId = OrderId("ORDER")
    val aAgentPath = AgentPath("A")
    val bAgentPath = AgentPath("B")

    "Branches start at different Agents" in:
      val workflow = Workflow.of(WorkflowPath("WORKFLOW") ~ "VERSION",
        Fork(Vector(
          "A" -> Workflow.of:
            Execute(WorkflowJob(aAgentPath, ShellScriptExecutable("?"))),
          "B" -> Workflow.of:
            Execute(WorkflowJob(bAgentPath, ShellScriptExecutable("?"))))))

      val coll = ControllerEventColl(
        ControllerState.forTest(workflows = Seq(workflow)),
        Timestamp.now)
      val result =
        locally:
          for
            coll <- coll:
              orderId <-: OrderAdded(workflow.id)
            coll <- coll:
              OrderEventSource.nextEvents(Set(orderId))
          yield coll
        .orThrow

      assert:
        result.keyedEventList == List(
          orderId <-: OrderAdded(workflow.id),
          orderId <-: OrderStarted,
          orderId <-: OrderForked(Vector(
            "A" -> orderId / "A",
            "B" -> orderId / "B")),
          orderId / "A" <-: OrderAttachable(aAgentPath),
          orderId / "B" <-: OrderAttachable(bAgentPath))

    "All branches start at the same Agent: fork at this Agent" in:
      // Fork instruction is executed at the Agent when all children are predicted
      // to be executed there and there a three or more children.
      val workflow = Workflow.of(WorkflowPath("WORKFLOW") ~ "VERSION",
        Fork(Vector(
          "A" -> Workflow.of:
            Execute(WorkflowJob(aAgentPath, ShellScriptExecutable("?"))),
          "B" -> Workflow.of:
            Execute(WorkflowJob(aAgentPath, ShellScriptExecutable("?"))),
          "C" -> Workflow.of:
            Execute(WorkflowJob(aAgentPath, ShellScriptExecutable("?"))))))

      val coll = ControllerEventColl(
        ControllerState.forTest(workflows = Seq(workflow)),
        Timestamp.now)
      val result =
        locally:
          for
            coll <- coll:
              orderId <-: OrderAdded(workflow.id)
            coll <- coll:
              OrderEventSource.nextEvents(Set(orderId))
          yield coll
        .orThrow
      assert:
        result.keyedEventList == List(
          orderId <-: OrderAdded(workflow.id),
          orderId <-: OrderAttachable(aAgentPath))
  }
