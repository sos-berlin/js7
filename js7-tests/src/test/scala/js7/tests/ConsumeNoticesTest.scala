package js7.tests

import java.time.LocalDate
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.TestMixins
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.board.BoardPathExpressionParser.boardPathExpr
import js7.data.board.{Board, BoardPath, BoardState, NoticeId}
import js7.data.controller.ControllerCommand.{CancelOrders, PostNotice}
import js7.data.order.OrderEvent.OrderNoticesExpected.Expected
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelled, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderNoticesConsumed, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{ConsumeNotices, Fail}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.ConsumeNoticesTest.*
import js7.tests.jobs.SemaphoreJob
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.View
import scala.concurrent.duration.*

final class ConsumeNoticesTest extends AnyFreeSpec with ControllerAgentForScalaTest
with BlockingItemUpdater with TestMixins
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 1ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected def agentPaths = Seq(agentPath)
  protected def items = Seq(board, board2)

  private val qualifiers = for (i <- Iterator.from(0)) yield
    LocalDate.of(3333, 3, 1).plusDays(i).toString

  "A single Order" in {
    // board2 is referenced but not required to have a Notice
    val workflow = updateItem(
      Workflow(WorkflowPath("CONSUMING-SINGLE"), Seq(
        ConsumeNotices(
          boardPathExpr(s"'${board.path.string}' || '${board2.path.string}'"),
          Workflow.of(TestJob.execute(agentPath))))))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    TestJob.reset()
    val orderId = OrderId(s"#$qualifier#CONSUMING")
    controllerApi
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticesExpected](_.key == orderId)

    controllerApi.executeCommand(
      PostNotice(board.path, noticeId)
    ).await(99.s).orThrow

    eventWatch.await[OrderNoticesConsumptionStarted](_.key == orderId)

    TestJob.continue()
    eventWatch.await[OrderNoticesConsumed](_.key == orderId)

    sleep(100.ms)
    assert(controllerState.keyTo(BoardState)(board.path).idToNotice.get(noticeId).isEmpty)
    assert(controllerState.keyTo(BoardState)(board2.path).idToNotice.get(noticeId).isEmpty)

    deleteItems(workflow.path)

    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(Expected(
        board.path, noticeId),
        Expected(board2.path, noticeId))),
      OrderNoticesConsumptionStarted(Vector(
        Expected(board.path, noticeId),
        Expected(board2.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 1),
      OrderDetachable, OrderDetached,
      OrderNoticesConsumed(),
      OrderFinished,
      OrderDeleted))
  }

  "A single Order waiting for one of two Notices, posting one Notice, then the other" in {
    val workflow = updateItem(
      Workflow(WorkflowPath("CONSUMING-SINGLE"), Seq(
        ConsumeNotices(
          boardPathExpr(s"'${board.path.string}' || '${board2.path.string}'"),
          Workflow.of(TestJob.execute(agentPath))))))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    TestJob.reset()
    val orderId = OrderId(s"#$qualifier#POST-UNUSED")
    controllerApi
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticesExpected](_.key == orderId)

    controllerApi.executeCommand(
      PostNotice(board.path, noticeId)
    ).await(99.s).orThrow

    eventWatch.await[OrderNoticesConsumptionStarted](_.key == orderId)

    sleep(100.ms)
    assert(controllerState.keyTo(BoardState)(board.path).idToNotice(noticeId).notice.isDefined)
    assert(controllerState.keyTo(BoardState)(board.path).idToNotice(noticeId).noticeIsInConsumption)
    assert(controllerState.keyTo(BoardState)(board.path).idToNotice(noticeId).consumptionCount == 1)

    // Notice in board2 is noticeIsInConsumption despite there was no notice
    assert(controllerState.keyTo(BoardState)(board2.path).idToNotice(noticeId).notice.isEmpty)
    assert(controllerState.keyTo(BoardState)(board2.path).idToNotice(noticeId).noticeIsInConsumption)
    assert(controllerState.keyTo(BoardState)(board2.path).idToNotice(noticeId).consumptionCount == 1)

    controllerApi.executeCommand(
      PostNotice(board2.path, noticeId)
    ).await(99.s).orThrow

    TestJob.continue()
    eventWatch.await[OrderNoticesConsumed](_.key == orderId)

    sleep(100.ms)
    // Notice at board has been deleted:
    assert(controllerState.keyTo(BoardState)(board.path).idToNotice.get(noticeId).isEmpty)

    // Notices posted at board2 while ConsumeNotices still exists:
    assert(controllerState.keyTo(BoardState)(board2.path).idToNotice(noticeId).notice.isDefined)
    assert(!controllerState.keyTo(BoardState)(board2.path).idToNotice(noticeId).noticeIsInConsumption)
    assert(controllerState.keyTo(BoardState)(board2.path).idToNotice(noticeId).consumptionCount == 0)

    deleteItems(workflow.path)

    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(Expected(
        board.path, noticeId),
        Expected(board2.path, noticeId))),
      OrderNoticesConsumptionStarted(Vector(
        Expected(board.path, noticeId),
        Expected(board2.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 1),
      OrderDetachable, OrderDetached,
      OrderNoticesConsumed(),
      OrderFinished,
      OrderDeleted))
  }

  "PostNotice while consuming an earlier notice" in {
    val workflow = updateItem(
      Workflow(WorkflowPath("POST-WHILE-CONSUMING"), Seq(
        ConsumeNotices(
          boardPathExpr(s"'${board.path.string}'"),
          Workflow.of(TestJob.execute(agentPath))))))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    controllerApi.executeCommand(
      PostNotice(board.path, noticeId)
    ).await(99.s).orThrow

    TestJob.reset()
    TestJob.continue()
    val orderId = OrderId(s"#$qualifier#CONSUMING")
    controllerApi
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticesConsumptionStarted](_.key == orderId)

    controllerApi.executeCommand(
      PostNotice(board.path, noticeId)
    ).await(99.s).orThrow
    eventWatch.await[OrderNoticesConsumed](_.key == orderId)

    sleep(100.ms)
    // The secondly posted Notice still exists:
    assert(controllerState.keyTo(BoardState)(board.path).idToNotice(noticeId).notice.isDefined)
    assert(!controllerState.keyTo(BoardState)(board.path).idToNotice(noticeId).noticeIsInConsumption)
    assert(controllerState.keyTo(BoardState)(board.path).idToNotice(noticeId).consumptionCount == 0)

    deleteItems(workflow.path)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesConsumptionStarted(Vector(Expected(board.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 1),
      OrderDetachable,
      OrderDetached,
      OrderNoticesConsumed(),
      OrderFinished,
      OrderDeleted))
  }

  "Two concurrent ConsumeNotices" in {
    val workflow = updateItem(
      Workflow(WorkflowPath("CONSUMING-TWO-ORDERS"), Seq(
        ConsumeNotices(
          boardPathExpr(s"'${board.path.string}'"),
          Workflow.of(TestJob.execute(agentPath))))))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    TestJob.reset()
    val aOrderId = OrderId(s"#$qualifier#CONSUMING-A")
    val bOrderId = OrderId(s"#$qualifier#CONSUMING-B")

    controllerApi.executeCommand(
      PostNotice(board.path, noticeId)
    ).await(99.s).orThrow

    for (orderId <- View(aOrderId, bOrderId)) {
      controllerApi
        .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.await[OrderNoticesConsumptionStarted](_.key == orderId)
    }

    sleep(100.ms)
    assert(controllerState.keyTo(BoardState)(board.path).idToNotice(noticeId).noticeIsInConsumption)
    assert(controllerState.keyTo(BoardState)(board.path).idToNotice(noticeId).consumptionCount == 2)

    TestJob.continue()
    eventWatch.await[OrderNoticesConsumed](_.key == aOrderId)
    sleep(100.ms)
    assert(controllerState.keyTo(BoardState)(board.path).idToNotice(noticeId).noticeIsInConsumption)
    assert(controllerState.keyTo(BoardState)(board.path).idToNotice(noticeId).consumptionCount == 1)

    TestJob.continue()
    eventWatch.await[OrderNoticesConsumed](_.key == aOrderId)
    sleep(100.ms)
    assert(controllerState.keyTo(BoardState)(board.path).idToNotice.get(noticeId).isEmpty)

    deleteItems(workflow.path)
    assert(eventWatch.eventsByKey[OrderEvent](aOrderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesConsumptionStarted(Vector(Expected(board.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 1),
      OrderDetachable,
      OrderDetached,
      OrderNoticesConsumed(),
      OrderFinished,
      OrderDeleted))

    assert(eventWatch.eventsByKey[OrderEvent](bOrderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesConsumptionStarted(Vector(Expected(board.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 1),
      OrderDetachable,
      OrderDetached,
      OrderNoticesConsumed(),
      OrderFinished,
      OrderDeleted))
  }

  "Failing ConsumeNotices block does not consume the Notice" in {
    val workflow = updateItem(
      Workflow(WorkflowPath("CONSUMING-FAILING"), Seq(
        ConsumeNotices(
          boardPathExpr(s"'${board.path.string}'"),
          Workflow.of(Fail())))))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    TestJob.reset()
    val orderId = OrderId(s"#$qualifier#CONSUMING-FAILING")
    controllerApi
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow

    controllerApi.executeCommand(
      PostNotice(board.path, noticeId)
    ).await(99.s).orThrow

    eventWatch.await[OrderNoticesConsumptionStarted](_.key == orderId)

    TestJob.continue()
    eventWatch.await[OrderNoticesConsumed](_.key == orderId)
    eventWatch.await[OrderFailed](_.key == orderId)

    sleep(100.ms)
    assert(controllerState.keyTo(BoardState)(board.path).idToNotice(noticeId).notice.isDefined)
    assert(!controllerState.keyTo(BoardState)(board.path).idToNotice(noticeId).noticeIsInConsumption)
    assert(controllerState.keyTo(BoardState)(board.path).idToNotice(noticeId).consumptionCount == 0)

    controllerApi.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    deleteItems(workflow.path)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(Expected(board.path, noticeId))),
      OrderNoticesConsumptionStarted(Vector(Expected(board.path, noticeId))),
      OrderNoticesConsumed(true),
      OrderFailed(Position(0), Some(Outcome.failed)),
      OrderCancelled,
      OrderDeleted))
  }
}

object ConsumeNoticesTest
{
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)

  // One lifeTime per board
  private val lifeTime = 1.day

  private val orderIdToNoticeId = expr(
    """replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$', '$1')""")

  private val board = Board(
    BoardPath(s"BOARD"),
    postOrderToNoticeId = orderIdToNoticeId,
    endOfLife = expr(s"$$js7EpochMilli + ${lifeTime.toMillis}"),
    expectOrderToNoticeId = orderIdToNoticeId)

  private val board2 = Board(
    BoardPath(s"BOARD-2"),
    postOrderToNoticeId = orderIdToNoticeId,
    endOfLife = expr(s"$$js7EpochMilli + ${lifeTime.toMillis}"),
    expectOrderToNoticeId = orderIdToNoticeId)

  final class TestJob extends SemaphoreJob(TestJob)
  private object TestJob extends SemaphoreJob.Companion[TestJob]
}
