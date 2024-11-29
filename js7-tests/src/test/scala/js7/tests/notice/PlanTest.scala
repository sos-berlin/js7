package js7.tests.notice

import fs2.Stream
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.item.ItemOperation
import js7.data.order.OrderEvent.{OrderPrompted, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.plan.{PlanTemplate, PlanTemplateId}
import js7.data.value.expression.ExpressionParser
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.Prompt
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.notice.PlanTest.*
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}

final class PlanTest
  extends OurTestSuite, ControllerAgentForScalaTest, BlockingItemUpdater:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 1ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected def agentPaths = Seq(agentPath)

  protected def items = Nil

  "PlanTemplate is only deletable if no Order is associated" in:
    withItem(Workflow.of(Prompt(expr("'PROMPT'")))): postingWorkflow =>
      eventWatch.resetLastWatchedEventId()

      val planTemplate = updateItem(PlanTemplate.joc(PlanTemplateId("DailyPlan")))
      val postingOrderId = OrderId("#2024-11-08#POST")
      controller.addOrderBlocking:
        FreshOrder(postingOrderId, postingWorkflow.path, deleteWhenTerminated = true)
      eventWatch.awaitNext[OrderPrompted](_.key == postingOrderId)

      locally:
        val checked = controller.api.updateItems(Stream:
          ItemOperation.Remove(planTemplate.path))
        .await(99.s)
        assert(checked == Left(Problem:
          "PlanTemplate:DailyPlan is in use by Plan:2024-11-08 (1 orders)"))

      execCmd:
        CancelOrders(Seq(postingOrderId))
      eventWatch.awaitNext[OrderTerminated](_.key == postingOrderId)

      deleteItems(planTemplate.path)


object PlanTest:

  private val agentPath = AgentPath("AGENT")
