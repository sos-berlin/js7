package js7.tests

import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.data.agent.AgentId
import js7.data.event.{EventSeq, KeyedEvent}
import js7.data.job.RelativeExecutablePath
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.NamedValues
import js7.data.workflow.instructions.Fork
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.FailUncatchableTest._
import js7.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

final class FailUncatchableTest extends AnyFreeSpec
{
  "fail" in {
    checkEvents[OrderFailed]("""
      |define workflow {
      |  execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
      |  fail (uncatchable=true);
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId),
        OrderAttachable(TestAgentId),
        OrderAttached(TestAgentId),
        OrderStarted,
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(1), Some(Outcome.failed))))
  }

  "fail (uncatchable=true, returnCode=7)" in {
    checkEvents[OrderFailed]("""
      |define workflow {
      |  execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
      |  fail (uncatchable=true, namedValues = { "returnCode": 7 });
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId),
        OrderAttachable(TestAgentId),
        OrderAttached(TestAgentId),
        OrderStarted,
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(1), Some(Outcome.Failed(NamedValues.rc(7))))))
  }

  "fail (uncatchable=true, returnCode=7, message='ERROR')" in {
    checkEvents[OrderFailed]("""
      |define workflow {
      |  execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
      |  fail (uncatchable=true, namedValues = { "returnCode": 7 }, message='TEST-ERROR');
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId),
        OrderAttachable(TestAgentId),
        OrderAttached(TestAgentId),
        OrderStarted,
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(1), Some(Outcome.Failed(Some("TEST-ERROR"), NamedValues.rc(7))))))
  }

  "fail in fork, fail first" in {
    val events = runUntil[OrderFailed]("""
     |define workflow {
     |  fork {
     |    "🥕": {
     |      execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
     |      fail (uncatchable=true, message="TEST-ERROR");
     |    },
     |    "🍋": {
     |      execute agent="AGENT", executable="sleep.cmd";
     |    }
     |  }
     |}""".stripMargin)

    assert(events.filter(_.key == orderId).map(_.event) ==
      Vector(
        OrderAdded(TestWorkflowId),
        OrderStarted,
        OrderForked(Vector(
          OrderForked.Child(Fork.Branch.Id("🥕"), OrderId("🔺|🥕")),
          OrderForked.Child(Fork.Branch.Id("🍋"), OrderId("🔺|🍋")))),
        OrderJoined(Outcome.failed),
        OrderFailed(Position(0))))

    assert(events.filter(_.key == (orderId | "🥕")).map(_.event) ==
      Vector(
        OrderAttachable(TestAgentId),
        OrderAttached(TestAgentId),
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderFailedInFork(Position(0) / BranchId.fork("🥕") % 1, Some(Outcome.Failed(Some("TEST-ERROR")))),
        OrderDetachable,
        OrderDetached))

    assert(events.filter(_.key == (orderId | "🍋")).map(_.event) ==
      Vector(
        OrderAttachable(TestAgentId),
        OrderAttached(TestAgentId),
        OrderProcessingStarted,
        OrderProcessed(Outcome.succeededRC0),
        OrderMoved(Position(0) / "fork+🍋" % 1),
        OrderDetachable,
        OrderDetached))
  }

  "fail in fork, succeed first" in {
    val events = runUntil[OrderFailed]("""
     |define workflow {
     |  fork {
     |    "🥕": {
     |      execute agent="AGENT", executable="sleep.cmd";
     |      fail (uncatchable=true, message="TEST-ERROR");
     |    },
     |    "🍋": {
     |      execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
     |    }
     |  }
     |}""".stripMargin)

    assert(events.filter(_.key == orderId).map(_.event) ==
      Vector(
        OrderAdded(TestWorkflowId),
        OrderStarted,
        OrderForked(Vector(
          OrderForked.Child(Fork.Branch.Id("🥕"), OrderId("🔺|🥕")),
          OrderForked.Child(Fork.Branch.Id("🍋"), OrderId("🔺|🍋")))),
        OrderJoined(Outcome.failed),
        OrderFailed(Position(0))))

    assert(events.filter(_.key == (orderId | "🥕")).map(_.event) ==
      Vector(
        OrderAttachable(TestAgentId),
        OrderAttached(TestAgentId),
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
        OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderFailedInFork(Position(0) / BranchId.fork("🥕") % 1, Some(Outcome.Failed(Some("TEST-ERROR")))),
        OrderDetachable,
        OrderDetached))

    assert(events.filter(_.key == (orderId | "🍋")).map(_.event) ==
      Vector(
        OrderAttachable(TestAgentId),
        OrderAttached(TestAgentId),
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
    autoClosing(new DirectoryProvider(TestAgentId :: Nil, workflow :: Nil, testName = Some("FailUncatchableTest"))) { directoryProvider =>
      directoryProvider.agents.head.writeExecutable(RelativeExecutablePath("test.cmd"), "exit 3")
      directoryProvider.agents.head.writeExecutable(RelativeExecutablePath("sleep.cmd"), DirectoryProvider.script(100.ms))
      directoryProvider.run { (controller, _) =>
        controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
        controller.eventWatch.await[E](_.key == orderId)
        controller.eventWatch.all[OrderEvent] match {
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
  private val TestAgentId = AgentId("AGENT")
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") ~ "INITIAL"
}
