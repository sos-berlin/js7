package js7.tests

import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.NamedValues
import js7.data.workflow.instructions.Fork
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{WorkflowParser, WorkflowPath}
import js7.tests.FailUncatchableTest.*
import js7.tests.testenv.DirectoryProvider
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.freespec.AnyFreeSpec
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.*

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
        OrderAttachable(TestAgentPath),
        OrderAttached(TestAgentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
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
        OrderAttachable(TestAgentPath),
        OrderAttached(TestAgentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
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
        OrderAttachable(TestAgentPath),
        OrderAttached(TestAgentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(1), Some(Outcome.Failed(Some("TEST-ERROR"), NamedValues.rc(7))))))
  }

  "fail in fork, fail first" in {
    val events = runUntil[OrderFailed]("""
     |define workflow {
     |  fork (joinIfFailed=true) {
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
        OrderJoined(Outcome.Failed(Some("Order:🔺|🥕 failed: TEST-ERROR"))),
        OrderFailed(Position(0))))

    assert(events.filter(_.key == orderId / "🥕").map(_.event) ==
      Vector(
        OrderAttachable(TestAgentPath),
        OrderAttached(TestAgentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(0) / "fork+🥕" % 1),
        // TODO OrderDetached, because agent does not has parent order and
        // cannot look at Fork.joinIfFailed. Okay because we join at Controller, anyway.
        OrderDetachable,
        OrderDetached,
        OrderFailedInFork(
          Position(0) / BranchId.fork("🥕") % 1,
          Some(Outcome.Failed(Some("TEST-ERROR"))))))
        //OrderDetachable,
        //OrderDetached))

    assert(events.filter(_.key == orderId / "🍋").map(_.event) ==
      Vector(
        OrderAttachable(TestAgentPath),
        OrderAttached(TestAgentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.succeededRC0),
        OrderMoved(Position(0) / "fork+🍋" % 1),
        OrderDetachable,
        OrderDetached))
  }

  "fail in fork, succeed first" in {
    val events = runUntil[OrderFailed]("""
     |define workflow {
     |  fork (joinIfFailed=true) {
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
        OrderJoined(Outcome.Failed(Some("Order:🔺|🥕 failed: TEST-ERROR"))),
        OrderFailed(Position(0))))

    assert(events.filter(_.key == orderId / "🥕").map(_.event) ==
      Vector(
        OrderAttachable(TestAgentPath),
        OrderAttached(TestAgentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(0))),
        OrderMoved(Position(0) / "fork+🥕" % 1),
        // TODO OrderDetached, because agent does not has parent order and
        // cannot look at Fork.joinIfFailed. Okay because we join at Controller, anyway.
        OrderDetachable,
        OrderDetached,
        OrderFailedInFork(
          Position(0) / BranchId.fork("🥕") % 1,
          Some(Outcome.Failed(Some("TEST-ERROR"))))))
        //OrderDetachable,
        //OrderDetached))

    assert(events.filter(_.key == orderId / "🍋").map(_.event) ==
      Vector(
        OrderAttachable(TestAgentPath),
        OrderAttached(TestAgentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(0) / "fork+🍋" % 1),
        OrderDetachable,
        OrderDetached))
  }

  private def checkEvents[E <: OrderEvent: ClassTag: TypeTag](workflowNotation: String, expectedEvents: Vector[OrderEvent]): Unit =
    assert(runUntil[E](workflowNotation).map(_.event) == expectedEvents)

  private def runUntil[E <: OrderEvent: ClassTag: TypeTag](workflowNotation: String): Vector[KeyedEvent[OrderEvent]] = {
    val workflow = WorkflowParser.parse(TestWorkflowId, workflowNotation).orThrow
    autoClosing(new DirectoryProvider(TestAgentPath :: Nil, workflow :: Nil, testName = Some("FailUncatchableTest"))) { directoryProvider =>
      directoryProvider.agents.head.writeExecutable(RelativePathExecutable("test.cmd"), "exit 3")
      directoryProvider.agents.head.writeExecutable(RelativePathExecutable("sleep.cmd"), DirectoryProvider.script(100.ms))
      directoryProvider.run { (controller, _) =>
        controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
        controller.eventWatch.await[E](_.key == orderId)
        controller.eventWatch
          .allKeyedEvents[OrderEvent]
          .view
          .filterNot(_.event.isInstanceOf[OrderStdWritten])
          .toVector
      }
    }
  }
}

object FailUncatchableTest
{
  private val orderId = OrderId("🔺")
  private val TestAgentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(TestAgentPath)
  private val TestWorkflowId = WorkflowPath("WORKFLOW") ~ "INITIAL"
}
