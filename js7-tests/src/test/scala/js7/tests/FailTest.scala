package js7.tests

import js7.base.problem.Checked.Ops
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.syntax._
import js7.common.system.OperatingSystem.isWindows
import js7.data.agent.AgentRefPath
import js7.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import js7.data.job.{ExecutablePath, ReturnCode}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderFailed, OrderFailedInFork, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderTransferredToAgent}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.FailTest._
import js7.tests.testenv.DirectoryProvider
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

final class FailTest extends AnyFreeSpec
{
  "fail" in {
    runUntil[OrderFailed]("""
      |define workflow {
      |  execute agent="/AGENT", executable="/test.cmd", successReturnCodes=[3];
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
        OrderFailed(Outcome.Failed(ReturnCode(3)))))
  }

  "fail (returnCode=7)" in {
    runUntil[OrderFailed]("""
      |define workflow {
      |  execute agent="/AGENT", executable="/test.cmd", successReturnCodes=[3];
      |  fail (returnCode=7);
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId),
        OrderAttachable(TestAgentRefPath),
        OrderTransferredToAgent(TestAgentRefPath),
        OrderStarted,
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(ReturnCode(3))),
        OrderMoved(Position(1)),
        OrderFailed(Outcome.Failed(ReturnCode(7)))))
  }

  "fail (returnCode=7, message='ERROR')" in {
    runUntil[OrderFailed]("""
      |define workflow {
      |  fail (returnCode=7, message='ERROR');
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId),
        OrderStarted,
        OrderFailed(Outcome.Failed(Some("ERROR"), ReturnCode(7)))))
  }

  "fail in fork" in {
    runUntil[OrderFailed]("""
      |define workflow {
      |  fork {
      |    "🥕": { execute agent="/AGENT", executable="/test.cmd", successReturnCodes=[3] },
      |    "🍋": { fail }
      |  }
      |}""".stripMargin,
      Vector(
        OrderAdded(TestWorkflowId),
        OrderStarted,
        OrderForked(Vector(
          OrderForked.Child("🥕", OrderId("🔺/🥕")),
          OrderForked.Child("🍋", OrderId("🔺/🍋")))),
        OrderJoined(Outcome.Failed(ReturnCode(0))),
        OrderFailed(Outcome.Failed(ReturnCode(0)))),
      OrderId("🔺/🍋") -> Vector(
        OrderFailedInFork(Outcome.Failed(None, ReturnCode(0)))))
  }

  private def runUntil[E <: OrderEvent: ClassTag: TypeTag](notation: String, expectedEvents: Vector[OrderEvent], moreExpectedEvents: (OrderId, Vector[OrderEvent])*): Unit =
    runUntil[E](
      WorkflowParser.parse(TestWorkflowId, notation).orThrow,
      expectedEvents,
      moreExpectedEvents: _*)

  private def runUntil[E <: OrderEvent: ClassTag: TypeTag](workflow: Workflow, expectedEvents: Vector[OrderEvent], moreExpectedEvents: (OrderId, Vector[OrderEvent])*): Unit =
    autoClosing(new DirectoryProvider(TestAgentRefPath :: Nil, workflow :: Nil, testName = Some("FailTest"))) { directoryProvider =>
      directoryProvider.agents.head.writeExecutable(ExecutablePath("/test.cmd"), (isWindows ?? "@echo off\n") + "exit 3")
      directoryProvider.run { (controller, _) =>
        val orderId = OrderId("🔺")
        controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
        controller.eventWatch.await[E](_.key == orderId)
        checkEventSeq(orderId, controller.eventWatch.all[OrderEvent], expectedEvents)
        for ((oId, expected) <- moreExpectedEvents) {
          checkEventSeq(oId, controller.eventWatch.all[OrderEvent], expected)
        }
      }
    }

  private def checkEventSeq(orderId: OrderId, eventSeq: TearableEventSeq[IterableOnce, KeyedEvent[OrderEvent]], expected: Vector[OrderEvent]): Unit =
    eventSeq match {
      case EventSeq.NonEmpty(stampeds) =>
        val events = stampeds.iterator.filter(_.value.key == orderId).map(_.value.event).to(Vector)
        assert(events == expected)
      case o =>
        fail(s"Unexpected EventSeq received: $o")
    }
}

object FailTest {
  private val TestAgentRefPath = AgentRefPath("/AGENT")
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") ~ "INITIAL"
}
