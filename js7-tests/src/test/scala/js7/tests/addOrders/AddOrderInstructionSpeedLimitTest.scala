package js7.tests.addOrders

import js7.base.configutils.Configs.*
import js7.base.problem.Checked.*
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.{OrderOrderAdded, OrderSleeping, OrderTerminated}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.value.expression.Expression.expr
import js7.data.workflow.instructions.AddOrder
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.addOrders.AddOrderInstructionSpeedLimitTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import org.scalatest.matchers.should.Matchers.*

final class AddOrderInstructionSpeedLimitTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.instruction.addOrder.speedLimit = [
      # First entry should be <= 10000 to keep generated event chunk small
      #{ limit: 3, period: 1ms }
      #{ limit: 10, period: 250ms }
      { limit: 1000, period: 1s }
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
    controller.eventWatch.allKeyedEvents[OrderOrderAdded].size shouldBe 3000

  "Explosing workflow" in:
    if !isIntelliJIdea then pending

    val orderId = OrderId("EXPLODING")
    addOrder(orderId, explodingWorkflow.path)
    sleep(1.s)
    val count = controller.eventWatch.allKeyedEvents[OrderOrderAdded].size
    assert(count <= 3000 + 1000 + 500) // (3000 for the first test + 500 due to testing delays)
    // Orders continue to start !!!

  "Many orders" in:
    if !isIntelliJIdea then pending
    // Run with -Djs7.noTest !!!
    val n = 100_000
    val orderIds = (1 to n).map(i => OrderId(s"MANY-$i"))
    controller.api.addOrders:
      orderIds.map: orderId =>
        FreshOrder(orderId, explodingWorkflow.path, deleteWhenTerminated = true)
    .await(99.s).orThrow
    sleep(if isIntelliJIdea then 10.s else 1.s)
    // Orders continue to start !!!


object AddOrderInstructionSpeedLimitTest:
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
      expr"""uniqueOrderId("DOUBLE-A-%d")""",
      explodingWorkflowPath,
      deleteWhenTerminated = true),
    AddOrder(
      expr"""uniqueOrderId("DOUBLE-B-%d")""",
      explodingWorkflowPath,
      deleteWhenTerminated = true))
