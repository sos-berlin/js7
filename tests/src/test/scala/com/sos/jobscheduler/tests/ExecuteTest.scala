package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.instructions.If.Then
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.tests.ExecuteTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec

final class ExecuteTest extends FreeSpec
{
  "Execute" in {
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, fileBased = TestWorkflow :: Nil)) { directoryProvider =>
      for (a <- directoryProvider.agents) {
        for (o <- Array("/SCRIPT-0a.cmd", "/SCRIPT-0b.cmd")) a.writeExecutable(ExecutablePath(o), ":")
        for (o <- Array("/SCRIPT-1.cmd", "/SCRIPT-2.cmd", "/SCRIPT-3.cmd", "/SCRIPT-4.cmd", "/SCRIPT-5.cmd"))
          a.writeExecutable(ExecutablePath(o),
            if (isWindows) "@exit %SCHEDULER_PARAM_RETURN_CODE%" else "exit $SCHEDULER_PARAM_RETURN_CODE")
      }
      directoryProvider.run { (master, _) =>
        val orderId = OrderId("🔺")
        master.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
        master.eventWatch.await[OrderFinished](_.key == orderId)
        checkEventSeq(orderId, master.eventWatch.all[OrderEvent])
      }
    }
  }

  private def checkEventSeq(orderId: OrderId, eventSeq: TearableEventSeq[TraversableOnce, KeyedEvent[OrderEvent]]): Unit = {
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) =>
        val events = stampeds.filter(_.value.key == orderId).map(_.value.event).toVector
        assert(events == ExpectedEvents)
      case o =>
        fail(s"Unexpected EventSeq received: $o")
    }
  }
}

object ExecuteTest {
  private val TestAgentRefPath = AgentRefPath("/AGENT")
  private val script = """
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
          execute executable="/SCRIPT-3.cmd", agent="AGENT", arguments={"return_code": "3"}, successReturnCodes=[3];
        }
        define job cJob {
          execute executable="/SCRIPT-4.cmd", agent="AGENT", arguments={"return_code": "4"}, successReturnCodes=[4];
        }
      };
      job dJob, arguments={"return_code": "5"};

      define job aJob {
        execute executable="/SCRIPT-0b.cmd", agent="AGENT";
      }
      define job bJob {
        execute executable="/SCRIPT-2.cmd", agent="AGENT", arguments={"return_code": "2"}, successReturnCodes=[2];
      }
      define job dJob {
        execute executable="/SCRIPT-5.cmd", agent="AGENT", arguments={"return_code": "99"}, successReturnCodes=[5];
      }
    }"""
  private val TestWorkflow = WorkflowParser.parse(WorkflowPath("/WORKFLOW") % "INITIAL", script).orThrow

  private val ExpectedEvents = Vector(
    OrderAdded(TestWorkflow.id, None),
    OrderAttachable(TestAgentRefPath),
    OrderTransferredToAgent(TestAgentRefPath),
    OrderStarted,
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(0))),
    OrderMoved(Position(1)),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(1))),
    OrderMoved(Position(2)),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(0))),
    OrderMoved(Position(3)),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(2))),
    OrderMoved(Position(4) / Then % 0),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(0))),
    OrderMoved(Position(4) / Then % 1),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(3))),
    OrderMoved(Position(4) / Then % 2),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(4))),
    OrderMoved(Position(5)),
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(5))),
    OrderMoved(Position(6)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderFinished)
}
