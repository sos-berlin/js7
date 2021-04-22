package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Checked.Ops
import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentPath
import js7.data.event.{EventSeq, KeyedEvent, TearableEventSeq}
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.NamedValues
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowParser, WorkflowPath}
import js7.tests.FailTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

final class FailTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentPaths = Seq(agentPath)
  protected val versionedItems = Nil
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """
  private val workflowIdIterator = Iterator.from(1).map(i => WorkflowPath(s"WORKFLOW-$i") ~ i.toString)
  private val orderIdIterator = Iterator.from(1).map(i => OrderId(s"♦️-$i"))

  override def beforeAll() = {
    directoryProvider.agents.head.writeExecutable(RelativePathExecutable("test.cmd"), (isWindows ?? "@echo off\n") + "exit 3")
    super.beforeAll()
  }

  "Fail" in {
    val workflowId = workflowIdIterator.next()
    val orderId = orderIdIterator.next()
    runUntil[OrderFailed](orderId, workflowId, """
      |define workflow {
      |  execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
      |  fail;
      |}""".stripMargin,
      Vector(
        OrderAdded(workflowId),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(1), Some(Outcome.failed))))
  }

  "Fail (returnCode=7)" in {
    val workflowId = workflowIdIterator.next()
    val orderId = orderIdIterator.next()
    runUntil[OrderFailed](orderId, workflowId, """
      |define workflow {
      |  execute agent="AGENT", executable="test.cmd", successReturnCodes=[3];
      |  fail (namedValues = { "returnCode": 7 });
      |}""".stripMargin,
      Vector(
        OrderAdded(workflowId),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted,
        OrderProcessed(Outcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(1), Some(Outcome.Failed(NamedValues.rc(7))))))
  }

  "Fail (returnCode=7, message='ERROR')" in {
    val workflowId = workflowIdIterator.next()
    val orderId = orderIdIterator.next()
    runUntil[OrderFailed](orderId, workflowId, """
      |define workflow {
      |  fail (namedValues = { "returnCode": 7 }, message='ERROR');
      |}""".stripMargin,
      Vector(
        OrderAdded(workflowId),
        OrderStarted,
        OrderFailed(Position(0), Some(Outcome.Failed(Some("ERROR"), NamedValues.rc(7))))))
  }

  "Fail in fork" in {
    val workflowId = workflowIdIterator.next()
    val orderId = OrderId("🔺")
    runUntil[OrderFailed](orderId, workflowId, """
      |define workflow {
      |  fork {
      |    "🥕": { execute agent="AGENT", executable="test.cmd", successReturnCodes=[3] },
      |    "🍋": { fail }
      |  }
      |}""".stripMargin,
      Vector(
        OrderAdded(workflowId),
        OrderStarted,
        OrderForked(Vector(
          OrderForked.Child("🥕", OrderId("🔺|🥕")),
          OrderForked.Child("🍋", OrderId("🔺|🍋")))),
        OrderJoined(Outcome.failed),
        OrderFailed(Position(0))),
      OrderId("🔺|🍋") -> Vector(
        OrderFailedInFork(Position(0) / BranchId.fork("🍋") % 0, Some(Outcome.failed))))
  }

  "Uncatchable fail in fork" in {
    val workflowId = workflowIdIterator.next()
    val orderId = OrderId("🟥")
    runUntil[OrderFailed](orderId, workflowId, """
      |define workflow {
      |  fork {
      |    "🥕": { execute agent="AGENT", executable="test.cmd", successReturnCodes=[3] },
      |    "🍋": { try { fail(uncatchable=true) } catch {}; }
      |  }
      |}""".stripMargin,
      Vector(
        OrderAdded(workflowId),
        OrderStarted,
        OrderForked(Vector(
          OrderForked.Child("🥕", OrderId("🟥|🥕")),
          OrderForked.Child("🍋", OrderId("🟥|🍋")))),
        OrderJoined(Outcome.failed),
        OrderFailed(Position(0))),
      OrderId("🟥|🍋") -> Vector(
        OrderMoved(Position(0) / "fork+🍋" % 0 / "try+0" % 0),
        OrderFailedInFork(Position(0) / BranchId.fork("🍋") % 0 / BranchId.try_(0) % 0, Some(Outcome.failed))))
  }

  private def runUntil[E <: OrderEvent: ClassTag: TypeTag](
    orderId: OrderId,
    workflowId: WorkflowId,
    workflowNotation: String,
    expectedEvents: Vector[OrderEvent],
    moreExpectedEvents: (OrderId, Vector[OrderEvent])*)
  : Unit =
    runUntil[E](
      orderId,
      WorkflowParser.parse(workflowId, workflowNotation).orThrow,
      expectedEvents,
      moreExpectedEvents: _*)

  private def runUntil[E <: OrderEvent: ClassTag: TypeTag](
    orderId: OrderId,
    workflow: Workflow,
    expectedEvents: Vector[OrderEvent],
    moreExpectedEvents: (OrderId, Vector[OrderEvent])*)
  : Unit = {
    directoryProvider.updateVersionedItems(controller, workflow.id.versionId, Seq(workflow))
    controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
    controller.eventWatch.await[E](_.key == orderId)
    checkEventSeq(orderId, controller.eventWatch.all[OrderEvent], expectedEvents)
    for ((oId, expected) <- moreExpectedEvents) {
      checkEventSeq(oId, controller.eventWatch.all[OrderEvent], expected)
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

object FailTest
{
  private val agentPath = AgentPath("AGENT")
}
