package js7.tests

import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.data.agent.AgentPath
import js7.data.event.{EventSeq, KeyedEvent}
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDeleted, OrderDetachable, OrderDetached, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.NamedValues
import js7.data.workflow.instructions.Fork
import js7.data.workflow.position.BranchId.Then
import js7.data.workflow.position.Position
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.FinishTest._
import js7.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

final class FinishTest extends AnyFreeSpec
{
  "finish" in {
    checkEvents[OrderFinished]("""
      |define workflow {
      |  execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
      |  finish;
      |  fail;
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId, deleteWhenTerminated = true),
        OrderAttachable(TestAgentPath),
        OrderAttached(TestAgentPath),
        OrderStarted,
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFinished,
        OrderDeleted))
  }

  "finish with if" in {
    checkEvents[OrderFinished]("""
      |define workflow {
      |  execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
      |  if (true) {
      |    execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
      |    finish;
      |  }
      |  fail;
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId, deleteWhenTerminated = true),
        OrderAttachable(TestAgentPath),
        OrderAttached(TestAgentPath),
        OrderStarted,
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1) / "then" % 0),
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1) / "then" % 1),
        OrderDetachable,
        OrderDetached,
        OrderFinished,
        OrderDeleted))
  }

  "finish in fork, finish first" in {
    val events = runUntil[OrderFinished]("""
     |define workflow {
     |  fork {
     |    "🥕": {
     |      execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
     |      if (true) {
     |        finish;
     |      }
     |      execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
     |     },
     |    "🍋": {
     |      execute agent="AGENT", executable="sleep.cmd";
     |    }
     |  }
     |}""".stripMargin)

    assert(events.filter(_.key == orderId).map(_.event) ==
      Vector(
        OrderAdded(TestWorkflowId, deleteWhenTerminated = true),
        OrderStarted,
        OrderForked(Vector(
          OrderForked.Child(Fork.Branch.Id("🥕"), OrderId("🔺|🥕")),
          OrderForked.Child(Fork.Branch.Id("🍋"), OrderId("🔺|🍋")))),
        OrderJoined(Outcome.succeeded),
        OrderMoved(Position(1)),
        OrderFinished,
        OrderDeleted))

    assert(events.filter(_.key == (orderId | "🥕")).map(_.event) ==
      Vector(
        OrderAttachable(TestAgentPath),
        OrderAttached(TestAgentPath),
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(0) / "fork+🥕" % 1 / Then % 0),  // Position of Finish
        OrderDetachable,
        OrderDetached,
        OrderMoved(Position(0) / "fork+🥕" % 3)))    // Moved to end

    assert(events.filter(_.key == (orderId | "🍋")).map(_.event) ==
      Vector(
        OrderAttachable(TestAgentPath),
        OrderAttached(TestAgentPath),
        OrderProcessingStarted,
        OrderProcessed(Outcome.succeededRC0),
        OrderMoved(Position(0) / "fork+🍋" % 1),
        OrderDetachable,
        OrderDetached))
  }

  "finish in fork, succeed first" in {
    val events = runUntil[OrderFinished]("""
     |define workflow {
     |  fork {
     |    "🥕": {
     |      execute agent="AGENT", executable="sleep.cmd";
     |      if (true) {
     |        finish;
     |      }
     |      execute agent="AGENT", executable="test.cmd";
     |    },
     |    "🍋": {
     |      execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
     |    }
     |  }
     |}""".stripMargin)

    assert(events.filter(_.key == orderId).map(_.event) ==
      Vector(
        OrderAdded(TestWorkflowId, deleteWhenTerminated = true),
        OrderStarted,
        OrderForked(Vector(
          OrderForked.Child(Fork.Branch.Id("🥕"), OrderId("🔺|🥕")),
          OrderForked.Child(Fork.Branch.Id("🍋"), OrderId("🔺|🍋")))),
        OrderJoined(Outcome.succeeded),
        OrderMoved(Position(1)),
        OrderFinished,
        OrderDeleted))

    assert(events.filter(_.key == (orderId | "🥕")).map(_.event) ==
      Vector(
        OrderAttachable(TestAgentPath),
        OrderAttached(TestAgentPath),
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
        OrderMoved(Position(0) / "fork+🥕" % 1 / Then % 0),  // Position of Finish
        OrderDetachable,
        OrderDetached,
        OrderMoved(Position(0) / "fork+🥕" % 3)))  // Moved to end

    assert(events.filter(_.key == (orderId | "🍋")).map(_.event) ==
      Vector(
        OrderAttachable(TestAgentPath),
        OrderAttached(TestAgentPath),
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(0) / "fork+🍋" % 1),
        OrderDetachable,
        OrderDetached))
  }


  private def checkEvents[E <: OrderEvent: ClassTag: TypeTag](workflowNotation: String, expectedEvents: Vector[OrderEvent]): Unit =
    assert(runUntil[E](workflowNotation).map(_.event) == expectedEvents)

  private def runUntil[E <: OrderEvent: ClassTag: TypeTag](workflowNotation: String): Vector[KeyedEvent[OrderEvent]] = {
    val workflow = WorkflowParser.parse(TestWorkflowId, workflowNotation).orThrow
    autoClosing(new DirectoryProvider(Seq(TestAgentPath), Seq(workflow), testName = Some("FinishTest"))) { directoryProvider =>
      directoryProvider.agents.head.writeExecutable(RelativePathExecutable("test.cmd"), "exit 3")
      directoryProvider.agents.head.writeExecutable(RelativePathExecutable("sleep.cmd"), DirectoryProvider.script(100.ms))
      directoryProvider.run { (controller, _) =>
        controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path, deleteWhenTerminated = true))
        controller.eventWatch.await[E](_.key == orderId)
        controller.eventWatch.await[OrderDeleted](_.key == orderId)
        controller.eventWatch.all[OrderEvent] match {
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
  private val TestAgentPath = AgentPath("AGENT")
  private val TestWorkflowId = WorkflowPath("WORKFLOW") ~ "INITIAL"
}
