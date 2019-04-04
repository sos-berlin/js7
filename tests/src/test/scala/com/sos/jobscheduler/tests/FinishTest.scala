package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.tests.FinishTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.reflect.ClassTag

final class FinishTest extends FreeSpec
{
  "finish" in {
    runUntil[OrderFinished]("""
      |define workflow {
      |  execute agent="/AGENT", executable="/test.cmd", successReturnCodes=[3];
      |  finish;
      |  fail;
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId),
        OrderAttachable(TestAgentRefPath),
        OrderTransferredToAgent(TestAgentRefPath),
        OrderStarted,
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(ReturnCode(3))),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderTransferredToMaster,
        OrderFinished))
  }

  private def runUntil[E <: OrderEvent: ClassTag](notation: String, expectedEvents: Vector[OrderEvent]): Unit =
    runUntil[E](
      WorkflowParser.parse(TestWorkflowId, notation).orThrow,
      expectedEvents)

  private def runUntil[E <: OrderEvent: ClassTag](workflow: Workflow, expectedEvents: Vector[OrderEvent]): Unit =
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, workflow :: Nil)) { directoryProvider =>
      directoryProvider.agents.head.writeExecutable(ExecutablePath("/test.cmd"), "exit 3")
      directoryProvider.run { (master, _) =>
        val orderId = OrderId("🔺")
        master.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
        master.eventWatch.await[E](_.key == orderId)
        checkEventSeq(orderId, master.eventWatch.all[OrderEvent], expectedEvents)
      }
    }

  private def checkEventSeq(orderId: OrderId, eventSeq: TearableEventSeq[TraversableOnce, KeyedEvent[OrderEvent]], expected: Vector[OrderEvent]): Unit = {
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) =>
        val events = stampeds.filter(_.value.key == orderId).map(_.value.event).toVector
        assert(events == expected)
      case o =>
        fail(s"Unexpected EventSeq received: $o")
    }
  }
}

object FinishTest {
  private val TestAgentRefPath = AgentRefPath("/AGENT")
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") ~ "INITIAL"
}
