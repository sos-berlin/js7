package com.sos.jobscheduler.tests

import akka.actor.ActorSystem
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.{Position, WorkflowPath}
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.tests.ExecuteTest._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.language.higherKinds

final class ExecuteTest extends FreeSpec {

  "Execute" in {
    autoClosing(new DirectoryProvider(List(TestAgentPath))) { directoryProvider ⇒
      directoryProvider.master.writeJson(TestWorkflow.withoutVersion)
      for (a ← directoryProvider.agents) {
        a.writeExecutable(ExecutablePath("/SCRIPT"), ":")
        a.writeExecutable(ExecutablePath("/SCRIPT-RC"),
          if (isWindows) "@exit %SCHEDULER_PARAM_RETURN_CODE%" else "exit $SCHEDULER_PARAM_RETURN_CODE")
      }
      directoryProvider.run { (master, _) ⇒
        val eventCollector = new TestEventCollector
        eventCollector.start(master.injector.instance[ActorSystem], master.injector.instance[StampedKeyedEventBus])
          val orderId = OrderId("🔺")
          master.addOrderBlocking(FreshOrder(orderId, TestWorkflow.id.path))
          eventCollector.await[OrderFinished](_.key == orderId)
          checkEventSeq(orderId, eventCollector.all[OrderEvent])
      }
    }
  }

  private def checkEventSeq(orderId: OrderId, eventSeq: TearableEventSeq[TraversableOnce, KeyedEvent[OrderEvent]]): Unit = {
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) ⇒
        val events = stampeds.filter(_.value.key == orderId).map(_.value.event).toVector
        assert(events == ExpectedEvents)
      case o ⇒
        fail(s"Unexpected EventSeq received: $o")
    }
  }
}

object ExecuteTest {
  private val TestAgentPath = AgentPath("/AGENT")
  private val script = """
    workflow {
      execute executable="/SCRIPT", agent="AGENT";
      execute executable="/SCRIPT-RC", agent="AGENT", arguments={"RETURN_CODE": "1"}, successReturnCodes=[0, 1];
      job aJob;
      job bJob;

      define job aJob {
        execute executable="/SCRIPT", agent="AGENT";
      }
      define job bJob {
        execute executable="/SCRIPT-RC", agent="AGENT", arguments={"RETURN_CODE": "1"}, successReturnCodes=[0, 1];
      }
    }"""
  private val TestWorkflow = WorkflowParser.parse(WorkflowPath("/WORKFLOW") % "(initial)", script).orThrow

  private val ExpectedEvents = Vector(
    OrderAdded(TestWorkflow.id, None),
    OrderTransferredToAgent(TestAgentPath % "(initial)"),
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
    OrderProcessed(MapDiff.empty, Outcome.Succeeded(ReturnCode(1))),
    OrderMoved(Position(4)),
    OrderDetachable,
    OrderTransferredToMaster,
    OrderFinished)
}
