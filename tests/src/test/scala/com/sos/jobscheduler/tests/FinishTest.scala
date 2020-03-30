package com.sos.jobscheduler.tests

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{EventSeq, KeyedEvent}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderDetachable, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.instructions.Fork
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.position.BranchId.Then
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.tests.FinishTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

final class FinishTest extends FreeSpec
{
  "finish" in {
    checkEvents[OrderFinished]("""
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

  "finish in fork, finish first" in {
    val events = runUntil[OrderFinished]("""
     |define workflow {
     |  fork {
     |    "🥕": {
     |      execute agent="/AGENT", executable="/test.cmd", successReturnCodes=[3];
     |      if (true) {
     |        finish;
     |      }
     |      execute agent="/AGENT", executable="/test.cmd", successReturnCodes=[3];
     |     },
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
        OrderMoved(Position(0) / "fork+🥕" % 1 / Then % 0),  // Position of Finish
        OrderDetachable,
        OrderTransferredToMaster,
        OrderMoved(Position(0) / "fork+🥕" % 3)))    // Moved to end

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

  "finish in fork, succeed first" in {
    val events = runUntil[OrderFinished]("""
     |define workflow {
     |  fork {
     |    "🥕": {
     |      execute agent="/AGENT", executable="/sleep.cmd";
     |      if (true) {
     |        finish;
     |      }
     |      execute agent="/AGENT", executable="/test.cmd";
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
        OrderMoved(Position(0) / "fork+🥕" % 1 / Then % 0),  // Position of Finish
        OrderDetachable,
        OrderTransferredToMaster,
        OrderMoved(Position(0) / "fork+🥕" % 3)))  // Moved to end

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


  private def checkEvents[E <: OrderEvent: ClassTag: TypeTag](workflowNotation: String, expectedEvents: Vector[OrderEvent]): Unit =
    assert(runUntil[E](workflowNotation).map(_.event) == expectedEvents)

  private def runUntil[E <: OrderEvent: ClassTag: TypeTag](workflowNotation: String): Vector[KeyedEvent[OrderEvent]] = {
    val workflow = WorkflowParser.parse(TestWorkflowId, workflowNotation).orThrow
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, workflow :: Nil, testName = Some("FinishTest"))) { directoryProvider =>
      directoryProvider.agents.head.writeExecutable(ExecutablePath("/test.cmd"), "exit 3")
      directoryProvider.agents.head.writeExecutable(ExecutablePath("/sleep.cmd"), DirectoryProvider.script(100.ms))
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

object FinishTest
{
  private val orderId = OrderId("🔺")
  private val TestAgentRefPath = AgentRefPath("/AGENT")
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") ~ "INITIAL"
}
