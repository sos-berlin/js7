package js7.tests

import js7.base.problem.Checked.Ops
import js7.base.utils.AutoClosing.autoClosing
import js7.data.agent.AgentName
import js7.data.event.{EventSeq, KeyedEvent}
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.NamedValues
import js7.data.workflow.WorkflowPath
import js7.data.workflow.instructions.Fork
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.position.Position
import js7.tests.FailUncatchableTest._
import js7.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

final class FailUncatchableTest extends AnyFreeSpec
{
  "fail" in {
    checkEvents[OrderFailed]("""
      |define workflow {
      |  execute agent="AGENT", executable="/test.cmd", successReturnCodes=[3];
      |  fail (uncatchable=true);
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId),
        OrderAttachable(TestAgentName),
        OrderAttached(TestAgentName),
        OrderStarted,
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Some(Outcome.failed))))
  }

  "fail (uncatchable=true, returnCode=7)" in {
    checkEvents[OrderFailed]("""
      |define workflow {
      |  execute agent="AGENT", executable="/test.cmd", successReturnCodes=[3];
      |  fail (uncatchable=true, namedValues = { "returnCode": 7 });
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId),
        OrderAttachable(TestAgentName),
        OrderAttached(TestAgentName),
        OrderStarted,
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Some(Outcome.Failed(NamedValues.rc(7))))))
  }

  "fail (uncatchable=true, returnCode=7, message='ERROR')" in {
    checkEvents[OrderFailed]("""
      |define workflow {
      |  execute agent="AGENT", executable="/test.cmd", successReturnCodes=[3];
      |  fail (uncatchable=true, namedValues = { "returnCode": 7 }, message='TEST-ERROR');
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId),
        OrderAttachable(TestAgentName),
        OrderAttached(TestAgentName),
        OrderStarted,
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Some(Outcome.Failed(Some("TEST-ERROR"), NamedValues.rc(7))))))
  }

  "fail in fork, fail first" in {
    val events = runUntil[OrderFailed]("""
     |define workflow {
     |  fork {
     |    "🥕": {
     |      execute agent="AGENT", executable="/test.cmd", successReturnCodes=[3];
     |      fail (uncatchable=true, message="TEST-ERROR");
     |    },
     |    "🍋": {
     |      execute agent="AGENT", executable="/sleep.cmd";
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
        OrderFailed()))

    assert(events.filter(_.key == (orderId | "🥕")).map(_.event) ==
      Vector(
        OrderAttachable(TestAgentName),
        OrderAttached(TestAgentName),
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderFailedInFork(Some(Outcome.Failed(Some("TEST-ERROR")))),
        OrderDetachable,
        OrderDetached))

    assert(events.filter(_.key == (orderId | "🍋")).map(_.event) ==
      Vector(
        OrderAttachable(TestAgentName),
        OrderAttached(TestAgentName),
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
     |      execute agent="AGENT", executable="/sleep.cmd";
     |      fail (uncatchable=true, message="TEST-ERROR");
     |    },
     |    "🍋": {
     |      execute agent="AGENT", executable="/test.cmd", successReturnCodes=[3];
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
        OrderFailed()))

    assert(events.filter(_.key == (orderId | "🥕")).map(_.event) ==
      Vector(
        OrderAttachable(TestAgentName),
        OrderAttached(TestAgentName),
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
        OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderFailedInFork(Some(Outcome.Failed(Some("TEST-ERROR")))),
        OrderDetachable,
        OrderDetached))

    assert(events.filter(_.key == (orderId | "🍋")).map(_.event) ==
      Vector(
        OrderAttachable(TestAgentName),
        OrderAttached(TestAgentName),
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
    autoClosing(new DirectoryProvider(TestAgentName :: Nil, workflow :: Nil, testName = Some("FailUncatchableTest"))) { directoryProvider =>
      directoryProvider.agents.head.writeExecutable(ExecutablePath("/test.cmd"), "exit 3")
      directoryProvider.agents.head.writeExecutable(ExecutablePath("/sleep.cmd"), DirectoryProvider.script(100.milliseconds))
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
  private val TestAgentName = AgentName("AGENT")
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") ~ "INITIAL"
}
