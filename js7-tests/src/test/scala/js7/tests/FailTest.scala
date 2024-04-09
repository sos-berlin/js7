package js7.tests
import izumi.reflect.Tag
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Checked.Ops
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.agent.AgentPath
import js7.data.event.KeyedEvent
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCaught, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.value.NamedValues
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.instructions.{Fail, TryInstruction}
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowParser, WorkflowPath}
import js7.tests.FailTest.*
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import cats.effect.unsafe.IORuntime
import org.scalactic.source
import scala.reflect.ClassTag

final class FailTest
extends OurTestSuite, ControllerAgentForScalaTest, BlockingItemUpdater:

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  private val workflowIdIterator = Iterator.from(1).map(i => WorkflowPath(s"WORKFLOW-$i") ~ i.toString)
  private val orderIdIterator = Iterator.from(1).map(i => OrderId(s"♦️-$i"))

  override def beforeAll() =
    directoryProvider.agentEnvs.head
      .writeExecutable(RelativePathExecutable("test.cmd"), (isWindows ?? "@echo off\n") + "exit 3")
    super.beforeAll()

  "Fail" in:
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
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1)),
        OrderOutcomeAdded(OrderOutcome.failed),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(1))))

  "Fail and Catch" in:
    val workflow = Workflow.of(WorkflowPath("FAIL-AND-CATCH"),
      TryInstruction(
        Workflow.of(
          Fail()),
        Workflow.empty))

    withTemporaryItem(workflow) { workflow =>
      val orderId = OrderId("FAIL-AND-CATCH")
      controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.await[OrderDeleted](_.key == orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) ==
        Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderMoved(Position(0) / "try+0" % 0),
          OrderStarted,
          OrderOutcomeAdded(OrderOutcome.failed),
          OrderCaught(Position(0) / "catch+0" % 0),
          OrderMoved(Position(1)),
          OrderFinished(),
          OrderDeleted))
    }

  "Fail (returnCode=7)" in:
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
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.Succeeded(NamedValues.rc(3))),
        OrderMoved(Position(1)),
        OrderOutcomeAdded(OrderOutcome.Failed(NamedValues.rc(7))),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(1))))

  "Fail (returnCode=7, message='ERROR')" in:
    val workflowId = workflowIdIterator.next()
    val orderId = orderIdIterator.next()
    runUntil[OrderFailed](orderId, workflowId, """
      |define workflow {
      |  fail (namedValues = { "returnCode": 7 }, message='ERROR');
      |}""".stripMargin,
      Vector(
        OrderAdded(workflowId),
        OrderStarted,
        OrderOutcomeAdded(OrderOutcome.Failed(Some("ERROR"), NamedValues.rc(7))),
        OrderFailed(Position(0))))

  "Fail in fork" in:
    val workflowId = workflowIdIterator.next()
    val orderId = OrderId("🔺")
    runUntil[OrderFailed](orderId, workflowId, """
      |define workflow {
      |  fork (joinIfFailed=true) {
      |    "🥕": { execute agent="AGENT", executable="test.cmd", successReturnCodes=[3] },
      |    "🍋": { fail }
      |  }
      |}""".stripMargin,
      Vector(
        OrderAdded(workflowId),
        OrderStarted,
        OrderForked(Vector(
          "🥕" -> OrderId("🔺|🥕"),
          "🍋" -> OrderId("🔺|🍋"))),
        OrderJoined(OrderOutcome.Failed(Some("Order:🔺|🍋 Failed"))),
        OrderFailed(Position(0))),
      OrderId("🔺|🍋") -> Vector(
        OrderOutcomeAdded(OrderOutcome.failed),
        OrderFailedInFork(Position(0) / BranchId.fork("🍋") % 0)))

  "Uncatchable fail in fork" in:
    val workflowId = workflowIdIterator.next()
    val orderId = OrderId("🟥")
    runUntil[OrderFailed](orderId, workflowId, """
      |define workflow {
      |  fork (joinIfFailed=true) {
      |    "🥕": { execute agent="AGENT", executable="test.cmd", successReturnCodes=[3] },
      |    "🍋": { try { fail(uncatchable=true) } catch {}; }
      |  }
      |}""".stripMargin,
      Vector(
        OrderAdded(workflowId),
        OrderStarted,
        OrderForked(Vector(
          "🥕" -> OrderId("🟥|🥕"),
          "🍋" -> OrderId("🟥|🍋"))),
        OrderJoined(OrderOutcome.Failed(Some("Order:🟥|🍋 Failed(uncatchable)"))),
        OrderFailed(Position(0))),
      OrderId("🟥|🍋") -> Vector(
        OrderMoved(Position(0) / "fork+🍋" % 0 / "try+0" % 0),
        OrderOutcomeAdded(OrderOutcome.failed.copy(uncatchable = true)),
        OrderFailedInFork(Position(0) / BranchId.fork("🍋") % 0 / BranchId.try_(0) % 0)))

  private def runUntil[E <: OrderEvent: ClassTag: Tag](
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
      moreExpectedEvents*)

  private def runUntil[E <: OrderEvent: ClassTag: Tag](
    orderId: OrderId,
    workflow: Workflow,
    expectedEvents: Vector[OrderEvent],
    moreExpectedEvents: (OrderId, Vector[OrderEvent])*)
  : Unit =
    directoryProvider.updateVersionedItems(controller, workflow.id.versionId, Seq(workflow))
    controller.addOrderBlocking(FreshOrder(orderId, workflow.id.path))
    eventWatch.await[E](_.key == orderId)
    checkEventSeq(orderId, eventWatch.allKeyedEvents[OrderEvent], expectedEvents)
    for (oId, expected) <- moreExpectedEvents do
      checkEventSeq(oId, eventWatch.allKeyedEvents[OrderEvent], expected)

  private def checkEventSeq(
    orderId: OrderId,
    keyedEvents: Seq[KeyedEvent[OrderEvent]],
    expected: Vector[OrderEvent])
    (implicit pos: source.Position)
  : Unit =
    val events = keyedEvents.view.filter(_.key == orderId).map(_.event).to(Vector)
    assert(events == expected)


object FailTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
