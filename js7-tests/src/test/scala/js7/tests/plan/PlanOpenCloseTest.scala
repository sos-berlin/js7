package js7.tests.plan

import js7.base.configutils.Configs.*
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.{CancelOrders, ChangePlanTemplate}
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.data.plan.{PlanTemplate, PlanTemplateId}
import js7.data.value.Value.convenience.given
import js7.data.value.expression.ExpressionParser.{expr, exprFunction}
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.Prompt
import js7.tests.plan.PlanOpenCloseTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.language.implicitConversions

final class PlanOpenCloseTest
  extends OurTestSuite, ControllerAgentForScalaTest:

  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  "Test" in {
    val planTemplate = PlanTemplate.joc(
      PlanTemplateId("DailyPlan"),
      planIsClosedFunction = Some(exprFunction("(day) => $day < $openingDay")))
    val workflow = Workflow.of(Prompt(expr("'PROMPT'")))

    withItems((planTemplate, workflow)): (planTemplate, workflow) =>
      eventWatch.resetLastWatchedEventId()

      val yesterday = "2024-12-02"
      val today = "2024-12-03"
      execCmd:
        ChangePlanTemplate(planTemplate.id, Map("openingDay" -> today))
      val yesterdayOrderId = OrderId(s"#$yesterday#")
      val todayOrderId = OrderId(s"#$today#")

      assert:
        controller.api.addOrder:
          FreshOrder(yesterdayOrderId, workflow.path, deleteWhenTerminated = true)
        .await(99.s)
          == Left(Problem("Plan:DailyPlan/2024-12-02 is closed"))

      controller.addOrderBlocking:
        FreshOrder(todayOrderId, workflow.path, deleteWhenTerminated = true)
      assert(controllerState.idToOrder(todayOrderId).isState[Order.Prompting])

      execCmd:
        CancelOrders(todayOrderId :: Nil)
  }


object PlanOpenCloseTest:
  private val agentPath = AgentPath("AGENT")
