package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Checked.Ops
import js7.data.agent.AgentPath
import js7.data.order.OrderEvent.OrderProcessed
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.launcher.configuration.Problems.SignedInjectionNotAllowed
import js7.tests.ExecuteNoScriptInjectionTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class ExecuteNoScriptInjectionTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(scriptWorkflow, absolutePathWorkflow)
  override protected val controllerConfig = config"""
    js7.journal.remove-obsolete-files = false"""
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = off
    """

  "signed-script-injection-allowed = off (default)" - {
    "Executing an ShellScriptExecutable is not allowed" in {
      testInjectionNotAllowed(OrderId("❌-ShellScriptExecutable"), scriptWorkflow.path)
    }

    "Executing an AbsolutePathExecutable is not allowed" in {
      testInjectionNotAllowed(OrderId("❌-AbsolutePathExecutable"), absolutePathWorkflow.path)
    }

    "Executing an CommandLineExecutable is not allowed" in {
      testInjectionNotAllowed(OrderId("❌-CommandLineExecutable"), commandLineWorkflow.path)
    }

    "Executing an InternalExecutable is not allowed" in {
      testInjectionNotAllowed(OrderId("❌-InternalExecutable"), internalJobWorkflow.path)
    }

    def testInjectionNotAllowed(orderId: OrderId, workflowPath: WorkflowPath): Unit = {
      controller.addOrderBlocking(FreshOrder(orderId, workflowPath))
      val orderProcessed = eventWatch.await[OrderProcessed](_.key == orderId).head
      assert(orderProcessed.value.event.outcome.asInstanceOf[Outcome.Disrupted].reason.problem ==
        SignedInjectionNotAllowed)
    }
  }
}

object ExecuteNoScriptInjectionTest
{
  private val agentPath = AgentPath("AGENT")

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

  private val commandLineWorkflow = WorkflowParser.parse(
    WorkflowPath("ABSOLUTE-PATH-WORKFLOW"),
    """define workflow {
        execute agent="AGENT", command="COMMAND LINE";
      }""").orThrow

  private val internalJobWorkflow = WorkflowParser.parse(
    WorkflowPath("ABSOLUTE-PATH-WORKFLOW"),
    """define workflow {
        execute agent="AGENT", internalJobClass="js7.executor.forjava.internal.tests.EmptyJInternalJob";
      }""").orThrow
}
