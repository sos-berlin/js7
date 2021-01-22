package js7.tests

import js7.agent.data.Problems.SignedInjectionNotAllowed
import js7.base.problem.Checked.Ops
import js7.common.configutils.Configs.HoconStringInterpolator
import js7.data.agent.AgentId
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.ExecuteNoScriptInjectionTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class ExecuteNoScriptInjectionTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = Seq(agentId)
  protected val versionedItems = Seq(scriptWorkflow, absolutePathWorkflow)
  override protected val controllerConfig = config"""
    js7.web.server.auth.public = on
    js7.journal.remove-obsolete-files = false"""
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = off
    """

  "signed-script-injection-allowed = off (default)" - {
    "Executing an inline script is not allowed" in {
      testInjectionNotAllowed(OrderId("❌"), scriptWorkflow.path)
    }

    "Executing an absolute path executable is not allowed" in {
      testInjectionNotAllowed(OrderId("❌❌"), absolutePathWorkflow.path)
    }

    def testInjectionNotAllowed(orderId: OrderId, workflowPath: WorkflowPath): Unit = {
      controller.addOrderBlocking(FreshOrder(orderId, workflowPath))
      val orderProcessed = controller.eventWatch.await[OrderProcessed](_.key == orderId).head
      assert(orderProcessed.value.event.outcome.asInstanceOf[Outcome.Disrupted].reason.problem == SignedInjectionNotAllowed)
    }
  }
}

object ExecuteNoScriptInjectionTest
{
  private val agentId = AgentId("AGENT")
  private val scriptWorkflow = WorkflowParser.parse(
    WorkflowPath("SCRIPT-WORKFLOW"),
    """define workflow {
         execute agent="AGENT", script=":";
       }""").orThrow
  private val absolutePathWorkflow = WorkflowParser.parse(
    WorkflowPath("ABSOLUTE-PATH-WORKFLOW"),
    """define workflow {
        execute agent="AGENT", executable="/ABSOLUTE";
      }""").orThrow
}
