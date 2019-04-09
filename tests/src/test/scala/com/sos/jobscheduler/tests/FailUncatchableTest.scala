package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.time.ScalaTime.JavaFiniteDuration
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderDetachable, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.instructions.Fork
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.tests.FailUncatchableTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.duration._
import scala.reflect.ClassTag

final class FailUncatchableTest extends FreeSpec
{
  "fail" in {
    checkEvents[OrderFailed]("""
      |define workflow {
      |  execute agent="/AGENT", executable="/test.cmd", successReturnCodes=[3];
      |  fail (uncatchable=true);
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
        OrderFailed(Outcome.Failed(ReturnCode(3)))))
  }

  "fail (uncatchable=true, returnCode=7)" in {
    checkEvents[OrderFailed]("""
      |define workflow {
      |  execute agent="/AGENT", executable="/test.cmd", successReturnCodes=[3];
      |  fail (uncatchable=true, returnCode=7);
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
        OrderFailed(Outcome.Failed(ReturnCode(7)))))
  }

  "fail (uncatchable=true, returnCode=7, message='ERROR')" in {
    checkEvents[OrderFailed]("""
      |define workflow {
      |  execute agent="/AGENT", executable="/test.cmd", successReturnCodes=[3];
      |  fail (uncatchable=true, returnCode=7, message='TEST-ERROR');
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
        OrderFailed(Outcome.Failed(Some("TEST-ERROR"), ReturnCode(7)))))
  }

  "fail in fork, fail first" in {
    val events = runUntil[OrderFinished]("""
     |define workflow {
     |  fork {
     |    "🥕": {
     |      execute agent="/AGENT", executable="/test.cmd", successReturnCodes=[3];
     |      fail (uncatchable=true, message="TEST-ERROR");
     |    },
     |    "🍋": {
     |      execute agent="/AGENT", executable="/sleep.cmd";
     |    }
     |  }
     |}""".stripMargin)

    assert(events.filter(_.key == orderId).map(_.event) ==
      Vector(
        OrderAdded(TestWorkflowId),
        OrderStarted,
        OrderForked(Vector(
          OrderForked.Child(Fork.Branch.Id("🥕"), OrderId("🔺/🥕")),
          OrderForked.Child(Fork.Branch.Id("🍋"), OrderId("🔺/🍋")))),
        OrderJoined(Outcome.succeeded),
        OrderMoved(Position(1)),
        OrderFinished))

    assert(events.filter(_.key == orderId / "🥕").map(_.event) ==
      Vector(
        OrderAttachable(TestAgentRefPath),
        OrderTransferredToAgent(TestAgentRefPath),
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(ReturnCode(3))),
        OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderFailedInFork(Outcome.Failed(Some("TEST-ERROR"), ReturnCode(3))),
        OrderDetachable,
        OrderTransferredToMaster))

    assert(events.filter(_.key == orderId / "🍋").map(_.event) ==
      Vector(
        OrderAttachable(TestAgentRefPath),
        OrderTransferredToAgent(TestAgentRefPath),
        OrderProcessingStarted,
        OrderProcessed(Outcome.succeeded),
        OrderMoved(Position(0) / "fork+🍋" % 1),
        OrderDetachable,
        OrderTransferredToMaster))
  }

  "fail in fork, succeed first" in {
    val events = runUntil[OrderFinished]("""
     |define workflow {
     |  fork {
     |    "🥕": {
     |      execute agent="/AGENT", executable="/sleep.cmd";
     |      fail (uncatchable=true, message="TEST-ERROR");
     |    },
     |    "🍋": {
     |      execute agent="/AGENT", executable="/test.cmd", successReturnCodes=[3];
     |    }
     |  }
     |}""".stripMargin)

    assert(events.filter(_.key == orderId).map(_.event) ==
      Vector(
        OrderAdded(TestWorkflowId),
        OrderStarted,
        OrderForked(Vector(
          OrderForked.Child(Fork.Branch.Id("🥕"), OrderId("🔺/🥕")),
          OrderForked.Child(Fork.Branch.Id("🍋"), OrderId("🔺/🍋")))),
        OrderJoined(Outcome.succeeded),
        OrderMoved(Position(1)),
        OrderFinished))

    assert(events.filter(_.key == orderId / "🥕").map(_.event) ==
      Vector(
        OrderAttachable(TestAgentRefPath),
        OrderTransferredToAgent(TestAgentRefPath),
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(ReturnCode(0))),
        OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderFailedInFork(Outcome.Failed(Some("TEST-ERROR"), ReturnCode(0))),
        OrderDetachable,
        OrderTransferredToMaster))

    assert(events.filter(_.key == orderId / "🍋").map(_.event) ==
      Vector(
        OrderAttachable(TestAgentRefPath),
        OrderTransferredToAgent(TestAgentRefPath),
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(ReturnCode(3))),
        OrderMoved(Position(0) / "fork+🍋" % 1),
        OrderDetachable,
        OrderTransferredToMaster))
  }

  private def checkEvents[E <: OrderEvent: ClassTag](workflowNotation: String, expectedEvents: Vector[OrderEvent]): Unit =
    assert(runUntil[E](workflowNotation).map(_.event) == expectedEvents)

  private def runUntil[E <: OrderEvent: ClassTag](workflowNotation: String): Vector[KeyedEvent[OrderEvent]] = {
    val workflow = WorkflowParser.parse(TestWorkflowId, workflowNotation).orThrow
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, workflow :: Nil)) { directoryProvider =>
      directoryProvider.agents.head.writeExecutable(ExecutablePath("/test.cmd"), "exit 3")
      directoryProvider.agents.head.writeExecutable(ExecutablePath("/sleep.cmd"), DirectoryProvider.script(100.milliseconds.toJavaDuration))
      directoryProvider.run { (master, _) =>
        master.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
        master.eventWatch.await[E](_.key == orderId)
        master.eventWatch.all[OrderEvent] match {
          case EventSeq.NonEmpty(stampeds) => stampeds.map(_.value).filterNot(_.event.isInstanceOf[OrderStdWritten]).toVector
          case o => fail(s"Unexpected EventSeq received: $o")
        }
      }
    }
  }
}

object FailUncatchableTest
{
  private val orderId = OrderId("🔺")
  private val TestAgentRefPath = AgentRefPath("/AGENT")
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") ~ "INITIAL"
}
