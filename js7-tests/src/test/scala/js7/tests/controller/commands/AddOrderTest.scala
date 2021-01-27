package js7.tests.controller.commands

import js7.base.problem.Checked.Ops
import js7.common.configutils.Configs._
import js7.controller.data.ControllerCommand.RemoveOrdersWhenTerminated
import js7.data.agent.AgentId
import js7.data.order.OrderEvent.{OrderAdded, OrderFinished, OrderRemoved, OrderStarted}
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.controller.commands.AddOrderTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class AddOrderTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = Seq(agentId)
  protected val versionedItems = Seq(emptyWorkflow)
  override protected val controllerConfig = config"""
    js7.web.server.auth.public = on
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
}

object AddOrderTest
{
  private val agentId = AgentId("AGENT")
  private val emptyWorkflow = Workflow.of(WorkflowPath("EMPTY"))
}
