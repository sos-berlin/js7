package js7.tests

import js7.agent.data.Problems.SignedInjectionNotAllowed
import js7.base.problem.Checked.Ops
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.system.OperatingSystem.isWindows
import js7.data.agent.AgentRefPath
import js7.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import js7.data.job.{ExecutablePath, ReturnCode}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderDetachable, OrderFailed, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderTransferredToAgent, OrderTransferredToController}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.position.BranchId.Then
import js7.data.workflow.position.Position
import js7.tests.ExecuteTest._
import js7.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class ExecuteTest extends AnyFreeSpec
{
  "Executing an inline script is not allowed" in {
    val workflowNotation = """
      define workflow {
        execute agent="AGENT", script=":";
      }"""
    val workflow = WorkflowParser.parse(WorkflowPath("/WORKFLOW"), workflowNotation).orThrow

    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, inventoryItems = workflow :: Nil, testName = Some("ExecuteTest"))) { directoryProvider =>
      directoryProvider.run { (controller, _) =>
        val orderId = OrderId("❌")
        controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
        val stampedSeq = controller.eventWatch.await[OrderFailed](_.key == orderId)
        assert(stampedSeq.head.value.event.outcome.asInstanceOf[Outcome.Disrupted].reason.problem == SignedInjectionNotAllowed)
      }
    }
  }

  "Execute" in {
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, inventoryItems = TestWorkflow :: Nil, testName = Some("ExecuteTest"))) { directoryProvider =>
      for (a <- directoryProvider.agents) {
        a.configDir / "agent.conf" ++= "js7.job.execution.signed-script-injection-allowed = on\n"
        for (o <- Array("/SCRIPT-0a.cmd", "/SCRIPT-0b.cmd")) a.writeExecutable(ExecutablePath(o), ":")
        for (o <- Array("/SCRIPT-1.cmd", "/SCRIPT-2.cmd", "/SCRIPT-3.cmd"))
          a.writeExecutable(ExecutablePath(o),
            if (isWindows) "@exit %SCHEDULER_PARAM_RETURN_CODE%" else "exit $SCHEDULER_PARAM_RETURN_CODE")
      }
      directoryProvider.run { (controller, _) =>
        val orderId = OrderId("🔺")
        controller.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
        controller.eventWatch.await[OrderFinished](_.key == orderId)
        checkEventSeq(orderId, controller.eventWatch.all[OrderEvent])
      }
    }
  }

  private def checkEventSeq(orderId: OrderId, eventSeq: TearableEventSeq[IterableOnce, KeyedEvent[OrderEvent]]): Unit = {
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) =>
        val events = stampeds.iterator.filter(_.value.key == orderId).map(_.value.event).to(Vector)
        assert(events == ExpectedEvents)
      case o =>
        fail(s"Unexpected EventSeq received: $o")
    }
  }
}

object ExecuteTest
{
  private val TestAgentRefPath = AgentRefPath("/AGENT")
  private val ScriptProlog = isWindows ?? "@echo off\n"
  private val workflowNotation = s"""
    define workflow {
      execute executable="/SCRIPT-0a.cmd", agent="AGENT";
      execute executable="/SCRIPT-1.cmd", agent="AGENT", arguments={"return_code": "1"}, successReturnCodes=[1];
      job aJob;
      job bJob;  // returnCode=2
      if (true) {
        job aJob;
        job bJob;  // returnCode=3
        job cJob;  // returnCode=4
        define job bJob {
          execute agent="AGENT", executable="/SCRIPT-3.cmd", arguments={"return_code": "3"}, successReturnCodes=[3];
        }
        define job cJob {
          execute agent="AGENT", script="${ScriptProlog}exit 4", arguments={"return_code": "4"}, successReturnCodes=[4];
        }
      };
      job dJob, arguments={"return_code": "5"};

      define job aJob {
        execute agent="AGENT", executable="/SCRIPT-0b.cmd";
      }
      define job bJob {
        execute agent="AGENT", executable="/SCRIPT-2.cmd", arguments={"return_code": "2"}, successReturnCodes=[2];
      }
      define job dJob {
        execute agent="AGENT", script="${ScriptProlog}exit 5", successReturnCodes=[5];
      }
    }"""
  private val TestWorkflow = WorkflowParser.parse(WorkflowPath("/WORKFLOW") ~ "INITIAL",  workflowNotation).orThrow

  private val ExpectedEvents = Vector(
    OrderAdded(TestWorkflow.id, None),
    OrderAttachable(TestAgentRefPath),
    OrderTransferredToAgent(TestAgentRefPath),
    OrderStarted,
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(ReturnCode(0))),
    OrderMoved(Position(1)),
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(ReturnCode(1))),
    OrderMoved(Position(2)),
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(ReturnCode(0))),
    OrderMoved(Position(3)),
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(ReturnCode(2))),
    OrderMoved(Position(4) / Then % 0),
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(ReturnCode(0))),
    OrderMoved(Position(4) / Then % 1),
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(ReturnCode(3))),
    OrderMoved(Position(4) / Then % 2),
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(ReturnCode(4))),
    OrderMoved(Position(5)),
    OrderProcessingStarted,
    OrderProcessed(Outcome.Succeeded(ReturnCode(5))),
    OrderMoved(Position(6)),
    OrderDetachable,
    OrderTransferredToController,
    OrderFinished)
}
