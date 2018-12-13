package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderStarted, OrderStopped}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.tests.FailTest._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.language.higherKinds
import scala.reflect.ClassTag

final class FailTest extends FreeSpec
{
  "fail" in {
    runUntil[OrderStopped]("""
      |define workflow {
      |  fail;
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId),
        OrderStarted,
        OrderStopped(Outcome.Failed(ReturnCode(-1)))))
  }

  "fail returnCode=7" in {
    runUntil[OrderStopped]("""
      |define workflow {
      |  fail returnCode=7;
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId),
        OrderStarted,
        OrderStopped(Outcome.Failed(ReturnCode(7)))))
  }

  private def runUntil[E <: OrderEvent: ClassTag](notation: String, expectedEvents: Vector[OrderEvent]): Unit =
    runUntil(
      WorkflowParser.parse(TestWorkflowId, notation).orThrow,
      expectedEvents)

  private def runUntil[E <: OrderEvent: ClassTag](workflow: Workflow, expectedEvents: Vector[OrderEvent]): Unit =
    autoClosing(new DirectoryProvider(List(TestAgentPath))) { directoryProvider ⇒
      directoryProvider.master.writeJson(workflow.withoutVersion)
      directoryProvider.run { (master, _) ⇒
        val orderId = OrderId("🔺")
        master.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
        master.eventWatch.await[E](_.key == orderId)
        checkEventSeq(orderId, master.eventWatch.all[OrderEvent], expectedEvents)
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

object FailTest {
  private val TestAgentPath = AgentPath("/AGENT")
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") % "(initial)"
}
