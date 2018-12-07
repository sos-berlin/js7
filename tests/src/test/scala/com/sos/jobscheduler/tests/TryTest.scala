package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension ⇒ sh}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderCatched, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStopped, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.tests.TryTest._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.language.higherKinds

final class TryTest extends FreeSpec
{
  "Nested try catch with outer non-failing catch, OrderFinished" in {
    autoClosing(new DirectoryProvider(List(TestAgentPath))) { directoryProvider ⇒
      directoryProvider.master.writeJson(FinishingWorkflow.withoutVersion)
      for (a ← directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/OKAY$sh"), ":")
      for (a ← directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/FAIL-1$sh"), if (isWindows) "@exit 1" else "exit 1")
      for (a ← directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/FAIL-2$sh"), if (isWindows) "@exit 2" else "exit 2")
      directoryProvider.run { (master, _) ⇒
        val orderId = OrderId("🔺")
        master.addOrderBlocking(FreshOrder(orderId, FinishingWorkflow.id.path))
        master.eventWatch.await[OrderFinished](_.key == orderId)
        checkEventSeq(orderId, master.eventWatch.all[OrderEvent], ExpectedFinishedEvents)
      }
    }
  }

  "Nested try catch with failing catch, OrderStopped" in {
    autoClosing(new DirectoryProvider(List(TestAgentPath))) { directoryProvider ⇒
      directoryProvider.master.writeJson(StoppedWorkflow.withoutVersion)
      for (a ← directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/FAIL-1$sh"), if (isWindows) "@exit 1" else "exit 1")
      for (a ← directoryProvider.agents) a.writeExecutable(ExecutablePath(s"/FAIL-2$sh"), if (isWindows) "@exit 2" else "exit 2")
      directoryProvider.run { (master, _) ⇒
        val orderId = OrderId("❌")
        master.addOrderBlocking(FreshOrder(orderId, StoppedWorkflow.id.path))
        master.eventWatch.await[OrderStopped](_.key == orderId)
        checkEventSeq(orderId, master.eventWatch.all[OrderEvent], ExpectedStoppedEvent)
      }
    }
  }

  private def checkEventSeq(orderId: OrderId, eventSeq: TearableEventSeq[TraversableOnce, KeyedEvent[OrderEvent]], expected: Vector[OrderEvent]): Unit = {
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) ⇒
        val events = stampeds.filter(_.value.key == orderId).map(_.value.event).toVector
        assert(events == expected)
      case o ⇒
        fail(s"Unexpected EventSeq received: $o")
    }
  }
}

object TryTest {
  private val TestAgentPath = AgentPath("/AGENT")
  private val finishingScript = s"""
     |define workflow {
     |  try {                                                 // #0
     |    try {                                               // #0/0/0
     |      execute executable="/FAIL-1$sh", agent="AGENT";   // #0/0/0/0/0   OrderCatched
     |      execute executable="/OKAY$sh", agent="AGENT";     // #0/0/0/0/1   skipped
     |    } catch {
     |      execute executable="/FAIL-2$sh", agent="AGENT";   // #0/0/0/1/0   OrderCatched
     |    };
     |    execute executable="/OKAY$sh", agent="AGENT";       // #0/0/1
     |  } catch {
     |    execute executable="/OKAY$sh", agent="AGENT";       // #0/1/0
     |  };
     |  execute executable="/OKAY$sh", agent="AGENT";         // #1
     |}""".stripMargin
  private val FinishingWorkflow = WorkflowParser.parse(WorkflowPath("/WORKFLOW") % "(initial)", finishingScript).orThrow

  private val ExpectedFinishedEvents = Vector(
    OrderAdded(FinishingWorkflow.id),
    OrderMoved(Position(0, 0, 0, 0, 0)),
    OrderAttachable(TestAgentPath),
    OrderTransferredToAgent(TestAgentPath % "(initial)"),

    OrderStarted,
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Failed(ReturnCode(1))),
    OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0, 0, 0, 1, 0)),

    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Failed(ReturnCode(2))),
    OrderCatched(Outcome.Failed(ReturnCode(2)), Position(0, 1, 0)),

    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.succeeded),
    OrderMoved(Position(1)),

    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.succeeded),
    OrderMoved(Position(2)),

    OrderDetachable,
    OrderTransferredToMaster,
    OrderFinished)

  private val stoppingScript = s"""
     |define workflow {
     |  try {                                               // #0
     |    execute executable="/FAIL-1$sh", agent="AGENT";   // #0/0/0  OrderCatched
     |  } catch {
     |    execute executable="/FAIL-2$sh", agent="AGENT";   // #0/1/0  OrderStopped
     |  };
     |}""".stripMargin
  private val StoppedWorkflow = WorkflowParser.parse(WorkflowPath("/WORKFLOW") % "(initial)", stoppingScript).orThrow

  private val ExpectedStoppedEvent = Vector(
    OrderAdded(FinishingWorkflow.id),
    OrderMoved(Position(0, 0, 0)),
    OrderAttachable(TestAgentPath),
    OrderTransferredToAgent(TestAgentPath % "(initial)"),

    OrderStarted,
    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Failed(ReturnCode(1))),
    OrderCatched(Outcome.Failed(ReturnCode(1)), Position(0, 1, 0)),

    OrderProcessingStarted,
    OrderProcessed(MapDiff.empty, Outcome.Failed(ReturnCode(2))),
    OrderStopped(Outcome.Failed(ReturnCode(2))))
}
