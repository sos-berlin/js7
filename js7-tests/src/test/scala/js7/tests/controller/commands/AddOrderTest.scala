package js7.tests.controller.commands

import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.common.configutils.Configs._
import js7.controller.data.ControllerCommand.RemoveOrdersWhenTerminated
import js7.data.agent.AgentId
import js7.data.job.ScriptExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderProcessed, OrderProcessingStarted, OrderRemoved, OrderStarted}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.expression.Expression.{NamedValue, ObjectExpression}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.commands.AddOrderTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class AddOrderTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = Seq(agentId)
  protected val versionedItems = Seq(emptyWorkflow, unknownArgWorkflow)
  override protected val controllerConfig = config"""
    js7.web.server.auth.public = on
    """
  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = yes
    """

  "Order in an empty workflow finishs immediately" in {
    val orderId = OrderId("EMPTY-WORKFLOW")
    assert(controller.runOrder(FreshOrder(orderId, emptyWorkflow.path)).map(_.value) == Seq(
      OrderAdded(emptyWorkflow.path ~ "INITIAL"),
      OrderStarted,
      OrderFinished))
    controller.executeCommandForTest(RemoveOrdersWhenTerminated(Seq(orderId))).orThrow
    controller.eventWatch.await[OrderRemoved](_.key == orderId)
  }

  "An unknown argument detected at Agent lets the order fail" in {
    for (i <- 1 to 2) {
      val orderId = OrderId(s"UNKNOWN-ARG-$i")
      assert(controller.runOrder(FreshOrder(orderId, unknownArgWorkflow.path)).map(_.value) == Seq(
        OrderAdded(unknownArgWorkflow.path ~ "INITIAL"),
        OrderAttachable(agentId),
        OrderAttached(agentId),
        OrderStarted,
        OrderProcessingStarted,
        OrderProcessed(Outcome.Disrupted(Problem("No such named value: string"))),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(0))))
    }
  }
}

object AddOrderTest
{
  private val agentId = AgentId("AGENT")
  private val emptyWorkflow = Workflow.of(WorkflowPath("EMPTY"))

  private val unknownArgWorkflow = Workflow(WorkflowPath("UNKNOWN-ARG"),
    labeledInstructions = Vector(
      Execute.Anonymous(WorkflowJob(agentId,
        ScriptExecutable(
          """#!/usr/bin/env bash
            |set -euo pipefail
            |echo "string=$string"
            |""".stripMargin,
          env = ObjectExpression(Map(
            "string" -> NamedValue.last("string"))))))
    ))
}
