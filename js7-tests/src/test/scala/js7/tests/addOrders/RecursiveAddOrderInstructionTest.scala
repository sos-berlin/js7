package js7.tests.addOrders

import js7.base.configutils.Configs.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.{OrderOrderAdded, OrderSleeping, OrderTerminated}
import js7.data.order.OrderId
import js7.data.value.expression.Expression.expr
import js7.data.workflow.instructions.AddOrder
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.addOrders.RecursiveAddOrderInstructionTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import org.scalatest.matchers.should.Matchers.*

final class RecursiveAddOrderInstructionTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.instruction.addOrder.speedLimit = [
      # First entry shold be <= 1000 to keep generated event chunk small
      { period: 1ms, limit: 3 },
      { period: 250ms, limit: 10 }
    ]
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(AgentPath("AGENT"))
  protected val items = Seq(simpleWorkflow, explodingWorkflow)

  "Test" in:
    val orderId = OrderId("FIRST")
    addOrder(orderId, simpleWorkflow.path)
    controller.awaitNextKey[OrderTerminated](orderId)
    controller.awaitNext[OrderSleeping]()
    controller.awaitNext[OrderSleeping]()
    controller.awaitNext[OrderSleeping]()
    controller.eventWatch.allKeyedEvents[OrderOrderAdded].size shouldBe 10

  "Heavy load" in:
    if !isIntelliJIdea then pending

    val orderId = OrderId("HEAVY")
    addOrder(orderId, explodingWorkflow.path)
    sleep(10.s)
    val count = controller.eventWatch.allKeyedEvents[OrderOrderAdded].size
    assert(count <= 10 * 44) // 40 orders/s, 10s + 1s for the first test
    // Orders continue to start !!!


object RecursiveAddOrderInstructionTest:
  private val simpleWorkflowPath = WorkflowPath.of("SIMPLE")
  private val simpleWorkflow = Workflow.of(simpleWorkflowPath,
    AddOrder(
      expr"""uniqueOrderId("ORDER-%d")""",
      simpleWorkflowPath,
      deleteWhenTerminated = true))

  private val explodingWorkflowPath = WorkflowPath.of("EXPLODING")

  // Each Order adds two Orders
  private val explodingWorkflow = Workflow.of(explodingWorkflowPath,
    AddOrder(
      expr"""uniqueOrderId("HEAVY-A-%d")""",
      explodingWorkflowPath,
      deleteWhenTerminated = true),
    AddOrder(
      expr"""uniqueOrderId("HEAVY-B-%d")""",
      explodingWorkflowPath,
      deleteWhenTerminated = true))
