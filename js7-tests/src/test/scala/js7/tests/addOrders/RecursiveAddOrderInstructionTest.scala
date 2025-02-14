package js7.tests.addOrders

import js7.base.configutils.Configs.*
import js7.base.test.OurTestSuite
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.OrderTerminated
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.expression.Expression.expr
import js7.data.workflow.instructions.AddOrder
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.testenv.ControllerAgentForScalaTest

final class RecursiveAddOrderInstructionTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(AgentPath("AGENT"))
  protected val items = Nil

  "Test" in:
    if !isIntelliJIdea then pending // FIXME Test fails because problem is not fixed

    val workflowPath = WorkflowPath.of("WORKFLOW")
    val workflow = Workflow.of(workflowPath,
      AddOrder(
        expr"'ORDER-' ++ (toNumber(replaceAll(orderId, '^ORDER-(.+)$$', '$$1')) + 1)",
        workflowPath))

    withItem(workflow): workflow =>
      val orderId = OrderId("ORDER-0")
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path))
      eventWatch.awaitNextKey[OrderTerminated](orderId)
