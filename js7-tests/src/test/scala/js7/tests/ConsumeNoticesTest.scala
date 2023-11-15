package js7.tests

import java.time.LocalDate
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.board.BoardPathExpressionParser.boardPathExpr
import js7.data.board.{Board, BoardPath, BoardState, Notice, NoticeId}
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, CancelOrders, DeleteNotice, PostNotice}
import js7.data.order.OrderEvent.OrderNoticesExpected.Expected
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelled, OrderCaught, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderNoticePosted, OrderNoticesConsumed, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderOperationCancelled, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderPromptAnswered, OrderPrompted, OrderRetrying, OrderStarted, OrderStdoutWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.StringValue
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{ConsumeNotices, Execute, Fail, PostNotices, Prompt, Retry, TryInstruction}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.ConsumeNoticesTest.*
import js7.tests.jobs.{FailingJob, SemaphoreJob}
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import monix.execution.Scheduler.Implicits.traced
import scala.collection.View
import scala.concurrent.duration.*

final class ConsumeNoticesTest extends OurTestSuite with ControllerAgentForScalaTest
with BlockingItemUpdater
{
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 1ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected def agentPaths = Seq(agentPath)
  protected def items = Seq(aBoard, bBoard)

  private val qualifiers = for (i <- Iterator.from(0)) yield
    LocalDate.of(3333, 1, 1).plusDays(i).toString

  "A single Order" in {
    // board2 is referenced but not required to have a Notice
    val workflow = updateItem(
      Workflow(WorkflowPath("CONSUMING-SINGLE"), Seq(
        ConsumeNotices(
          boardPathExpr(s"'${aBoard.path.string}' || '${bBoard.path.string}'"),
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
      PostNotice(aBoard.path, noticeId)
    ).await(99.s).orThrow

    eventWatch.await[OrderNoticesConsumptionStarted](_.key == orderId)

    TestJob.continue()
    eventWatch.await[OrderNoticesConsumed](_.key == orderId)

    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice.get(noticeId).isEmpty)
    assert(controllerState.keyTo(BoardState)(bBoard.path).idToNotice.get(noticeId).isEmpty)

    deleteItems(workflow.path)

    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(Expected(
        aBoard.path, noticeId),
        Expected(bBoard.path, noticeId))),
      OrderNoticesConsumptionStarted(Vector(
        Expected(aBoard.path, noticeId),
        Expected(bBoard.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 1),
      OrderDetachable, OrderDetached,
      OrderNoticesConsumed(),
      OrderFinished(),
      OrderDeleted))
  }

  "Simple test with two boards" in {
    val orderIdToNoticeId = expr(
      """replaceAll($js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#(.*)$', '$1-$2')""")

    val myBoard = Board(
      BoardPath("MY-BOARD"),
      postOrderToNoticeId = orderIdToNoticeId,
      endOfLife = expr(s"$$js7EpochMilli + ${lifetime.toMillis}"),
      expectOrderToNoticeId = orderIdToNoticeId)

    val workflow = Workflow(
      WorkflowPath("SIMPLE-WITH-TWO-BOARDS"), Seq(
        PostNotices(Seq(aBoard.path, myBoard.path)),
        ConsumeNotices(
          boardPathExpr(s"'${aBoard.path.string}' && '${myBoard.path.string}'"),
          Workflow.empty)))

    withTemporaryItem(myBoard) { _ =>
      withTemporaryItem(workflow) { workflow =>
        val orderId = OrderId("#2022-10-23#X")
        val events = controller.runOrder(
          FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        val endOfLife = Timestamp.Epoch
        assert(events.map(_.value).map {
          case e: OrderNoticePosted => e.copy(notice = e.notice.copy(endOfLife = endOfLife))
          case o => o
        } == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderStarted,
          OrderNoticePosted(Notice(NoticeId("2022-10-23"), aBoard.path, endOfLife)),
          OrderNoticePosted(Notice(NoticeId("2022-10-23-X"), myBoard.path, endOfLife)),
          OrderMoved(Position(1)),
          OrderNoticesConsumptionStarted(Vector(
            Expected(aBoard.path, NoticeId("2022-10-23")),
            Expected(myBoard.path, NoticeId("2022-10-23-X")))),
          OrderNoticesConsumed(false),
          OrderFinished(),
          OrderDeleted))
      }
    }
  }

  "ConsumeNotices with interfering DeleteNotice command" in {
    val workflow = Workflow(
      WorkflowPath("SIMPLE-WITH-TWO-BOARDS"), Seq(
        PostNotices(Seq(aBoard.path)),
        ConsumeNotices(
          boardPathExpr(s"'${aBoard.path.string}'"),
          Workflow.of(Prompt(expr("'PROMPT'"))))))

    withTemporaryItem(workflow) { workflow =>
      val eventId = eventWatch.lastAddedEventId
      val noticeId = NoticeId("2022-10-24")
      val orderId = OrderId(s"#${noticeId.string}#")
      controller.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)).await(99.s).orThrow
      eventWatch.await[OrderPrompted](_.key == orderId)

      controllerApi.executeCommand(DeleteNotice(aBoard.path, noticeId)).await(99.s).orThrow

      controllerApi.executeCommand(AnswerOrderPrompt(orderId)).await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId)

      val events = eventWatch.eventsByKey[OrderEvent](orderId, after = eventId)
      val endOfLife = Timestamp.Epoch
      assert(events.map {
        case e: OrderNoticePosted => e.copy(notice = e.notice.copy(endOfLife = endOfLife))
        case o => o
      } == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderNoticePosted(Notice(NoticeId("2022-10-24"), aBoard.path, endOfLife)),
        OrderMoved(Position(1)),
        OrderNoticesConsumptionStarted(Vector(
          Expected(aBoard.path, NoticeId("2022-10-24")))),
        OrderPrompted(StringValue("PROMPT")),
        OrderPromptAnswered(),
        OrderMoved(Position(1) / "consumeNotices" % 1),
        OrderNoticesConsumed(false),
        OrderFinished(),
        OrderDeleted))
    }
  }

  "A single Order waiting for one of two Notices, posting one Notice, then the other" in {
    val workflow = updateItem(
      Workflow(WorkflowPath("CONSUMING-SINGLE"), Seq(
        ConsumeNotices(
          boardPathExpr(s"'${aBoard.path.string}' || '${bBoard.path.string}'"),
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
      PostNotice(aBoard.path, noticeId)
    ).await(99.s).orThrow

    eventWatch.await[OrderNoticesConsumptionStarted](_.key == orderId)

    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).notice.isDefined)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).consumptionCount == 1)

    // Notice in board2 is isInConsumption despite there was no notice
    assert(controllerState.keyTo(BoardState)(bBoard.path).idToNotice(noticeId).notice.isEmpty)
    assert(controllerState.keyTo(BoardState)(bBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(bBoard.path).idToNotice(noticeId).consumptionCount == 1)

    controllerApi.executeCommand(
      PostNotice(bBoard.path, noticeId)
    ).await(99.s).orThrow

    TestJob.continue()
    eventWatch.await[OrderNoticesConsumed](_.key == orderId)

    // Notice at board has been deleted:
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice.get(noticeId).isEmpty)

    // Notices posted at board2 while ConsumeNotices still exists:
    assert(controllerState.keyTo(BoardState)(bBoard.path).idToNotice(noticeId).notice.isDefined)
    assert(!controllerState.keyTo(BoardState)(bBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(bBoard.path).idToNotice(noticeId).consumptionCount == 0)

    deleteItems(workflow.path)

    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(Expected(
        aBoard.path, noticeId),
        Expected(bBoard.path, noticeId))),
      OrderNoticesConsumptionStarted(Vector(
        Expected(aBoard.path, noticeId),
        Expected(bBoard.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 1),
      OrderDetachable, OrderDetached,
      OrderNoticesConsumed(),
      OrderFinished(),
      OrderDeleted))
  }

  "PostNotice while consuming an earlier notice" in {
    val workflow = updateItem(
      Workflow(WorkflowPath("POST-WHILE-CONSUMING"), Seq(
        ConsumeNotices(
          boardPathExpr(s"'${aBoard.path.string}'"),
          Workflow.of(TestJob.execute(agentPath))))))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    controllerApi.executeCommand(
      PostNotice(aBoard.path, noticeId)
    ).await(99.s).orThrow

    TestJob.reset()
    TestJob.continue()
    val orderId = OrderId(s"#$qualifier#CONSUMING")
    controllerApi
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticesConsumptionStarted](_.key == orderId)

    controllerApi.executeCommand(
      PostNotice(aBoard.path, noticeId)
    ).await(99.s).orThrow
    eventWatch.await[OrderNoticesConsumed](_.key == orderId)

    // The secondly posted Notice still exists:
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).notice.isDefined)
    assert(!controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).consumptionCount == 0)

    deleteItems(workflow.path)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesConsumptionStarted(Vector(Expected(aBoard.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 1),
      OrderDetachable,
      OrderDetached,
      OrderNoticesConsumed(),
      OrderFinished(),
      OrderDeleted))
  }

  "Two concurrent ConsumeNotices" in {
    val workflow = updateItem(
      Workflow(WorkflowPath("CONSUMING-TWO-ORDERS"), Seq(
        ConsumeNotices(
          boardPathExpr(s"'${aBoard.path.string}'"),
          Workflow.of(TestJob.execute(agentPath))))))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    TestJob.reset()
    val aOrderId = OrderId(s"#$qualifier#CONSUMING-A")
    val bOrderId = OrderId(s"#$qualifier#CONSUMING-B")

    controllerApi.executeCommand(
      PostNotice(aBoard.path, noticeId)
    ).await(99.s).orThrow

    for (orderId <- View(aOrderId, bOrderId)) {
      controllerApi
        .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.await[OrderNoticesConsumptionStarted](_.key == orderId)
    }

    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).consumptionCount == 2)

    TestJob.continue()
    eventWatch.await[OrderNoticesConsumed](_.key == aOrderId)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).consumptionCount == 1)

    TestJob.continue()
    eventWatch.await[OrderNoticesConsumed](_.key == aOrderId)
    awaitAndAssert(
      controllerState.keyTo(BoardState)(aBoard.path).idToNotice.get(noticeId).isEmpty)

    deleteItems(workflow.path)
    assert(eventWatch.eventsByKey[OrderEvent](aOrderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesConsumptionStarted(Vector(Expected(aBoard.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 1),
      OrderDetachable,
      OrderDetached,
      OrderNoticesConsumed(),
      OrderFinished(),
      OrderDeleted))

    assert(eventWatch.eventsByKey[OrderEvent](bOrderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesConsumptionStarted(Vector(Expected(aBoard.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 1),
      OrderDetachable,
      OrderDetached,
      OrderNoticesConsumed(),
      OrderFinished(),
      OrderDeleted))
  }

  "Nested ConsumeNotcies" in {
    val workflow = updateItem(
      Workflow(
        WorkflowPath("CONSUMING-NESTED"),
        Seq(
          ConsumeNotices(
            boardPathExpr(s"'${aBoard.path.string}'"),
            Workflow.of(
              ConsumeNotices(
                boardPathExpr(s"'${aBoard.path.string}' || '${bBoard.path.string}'"),
                Workflow.of(
                  Execute(WorkflowJob.Name("JOB")))),
              Prompt(expr("'PROMPT'"))))),
        nameToJob = Map(
          WorkflowJob.Name("JOB") -> TestJob.workflowJob(agentPath))))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    TestJob.reset()
    val orderId = OrderId(s"#$qualifier#NESTED")
    controllerApi
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticesExpected](_.key == orderId)

    controllerApi.executeCommand(
      PostNotice(aBoard.path, noticeId)
    ).await(99.s).orThrow

    eventWatch.await[OrderStdoutWritten](_.key == orderId)

    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).notice.isDefined)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).consumptionCount == 2)

    assert(controllerState.keyTo(BoardState)(bBoard.path).idToNotice(noticeId).notice.isEmpty)
    assert(controllerState.keyTo(BoardState)(bBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(bBoard.path).idToNotice(noticeId).consumptionCount == 1)

    TestJob.continue()
    eventWatch.await[OrderPrompted](_.key == orderId)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).notice.isDefined)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).consumptionCount == 1)

    assert(controllerState.keyTo(BoardState)(bBoard.path).idToNotice.get(noticeId).isEmpty)

    controllerApi.executeCommand(AnswerOrderPrompt(orderId)).await(99.s).orThrow
    eventWatch.await[OrderNoticesConsumed](_.key == orderId)

    deleteItems(workflow.path)

    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(
        Expected(aBoard.path, noticeId))),
      OrderNoticesConsumptionStarted(Vector(
        Expected(aBoard.path, noticeId))),
      OrderNoticesConsumptionStarted(Vector(
        Expected(aBoard.path, noticeId),
        Expected(bBoard.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 0 / "consumeNotices" % 1),
      OrderDetachable, OrderDetached,
      OrderNoticesConsumed(),
      OrderPrompted(StringValue("PROMPT")),
      OrderPromptAnswered(),
      OrderMoved(Position(0) / "consumeNotices" % 2),
      OrderNoticesConsumed(),
      OrderFinished(),
      OrderDeleted))
  }

  "Failing ConsumeNotices block does not consume the Notice" in {
    val workflow = updateItem(
      Workflow(WorkflowPath("CONSUMING-FAILING"), Seq(
        ConsumeNotices(
          boardPathExpr(s"'${aBoard.path.string}'"),
          Workflow.of(Fail())))))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    TestJob.reset()
    val orderId = OrderId(s"#$qualifier#CONSUMING-FAILING")
    controllerApi
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow

    controllerApi.executeCommand(
      PostNotice(aBoard.path, noticeId)
    ).await(99.s).orThrow

    eventWatch.await[OrderNoticesConsumptionStarted](_.key == orderId)

    TestJob.continue()
    eventWatch.await[OrderNoticesConsumed](_.key == orderId)
    eventWatch.await[OrderFailed](_.key == orderId)

    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).notice.isDefined)
    assert(!controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).consumptionCount == 0)

    controllerApi.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    deleteItems(workflow.path)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(Expected(aBoard.path, noticeId))),
      OrderNoticesConsumptionStarted(Vector(Expected(aBoard.path, noticeId))),
      OrderOutcomeAdded(Outcome.failed),
      OrderNoticesConsumed(true),
      OrderFailed(Position(0)),
      OrderCancelled,
      OrderDeleted))
  }

  "Cancel while consuming a Notice and sticking in Promting" in {
    val workflow = updateItem(
      Workflow(WorkflowPath("CANCEL-WHILE-PROMPTING"), Seq(
        ConsumeNotices(
          boardPathExpr(s"'${aBoard.path.string}'"),
          Workflow.of(Prompt(expr("'PROMPT'")))))))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    val orderId = OrderId(s"#$qualifier#CANCEL-WHILE-PROMTING")
    controllerApi
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow

    controllerApi.executeCommand(
      PostNotice(aBoard.path, noticeId)
    ).await(99.s).orThrow

    eventWatch.await[OrderPrompted](_.key == orderId)
    controllerApi.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow

    eventWatch.await[OrderNoticesConsumed](_.key == orderId)
    eventWatch.await[OrderCancelled](_.key == orderId)

    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).notice.isDefined)
    assert(!controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).consumptionCount == 0)

    deleteItems(workflow.path)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(Expected(aBoard.path, noticeId))),
      OrderNoticesConsumptionStarted(Vector(Expected(aBoard.path, noticeId))),
      OrderPrompted(StringValue("PROMPT")),
      OrderOperationCancelled,
      OrderNoticesConsumed(true),
      OrderCancelled,
      OrderDeleted))
  }

  "JS-2015 ConsumeOrders in Try/Retry with 0s delay" in {
    val noticeId = NoticeId("2022-10-25")
    val workflow = Workflow(
      WorkflowPath("CONSUME-NOTICES-IN-RETRY"),
      Seq(
        PostNotices(Seq(aBoard.path)),
        TryInstruction(
          Workflow.of(
            ConsumeNotices(
              boardPathExpr(s"'${aBoard.path.string}'"),
              Workflow.of(
                FailingJob.execute(agentPath)))),
          Workflow.of(
            Retry()),
          retryDelays = Some(Vector(0.s)),
          maxTries = Some(2))))

    withTemporaryItem(workflow) { workflow =>
      val events = controller.runOrder(FreshOrder(OrderId("#2022-10-25#"), workflow.path))
      val endOfLife = Timestamp.Epoch
      assert(events.map(_.value).map {
        case e: OrderNoticePosted => e.copy(notice = e.notice.copy(endOfLife = endOfLife))
        case o => o
      } == Seq(
        OrderAdded(workflow.id),
        OrderStarted,
        OrderNoticePosted(Notice(noticeId, aBoard.path, endOfLife)),
        OrderMoved(Position(1) / "try+0" % 0),

        OrderNoticesConsumptionStarted(Vector(Expected(aBoard.path, noticeId))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Failed(Some("💥FailingJob failed💥"))),
        OrderDetachable,
        OrderDetached,
        OrderNoticesConsumed(failed = true),

        OrderCaught(Position(1) / "catch+0" % 0),
        OrderRetrying(Position(1) / "try+1" % 0),

        OrderNoticesConsumptionStarted(Vector(Expected(aBoard.path, noticeId))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Failed(Some("💥FailingJob failed💥"))),
        OrderDetachable,
        OrderDetached,
        OrderNoticesConsumed(failed = true),

        OrderFailed(Position(1) / "try+1" % 0)))
    }
  }
}

object ConsumeNoticesTest
{
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)

  // One lifetime per board
  private val lifetime = 1.day
  private val aBoard = Board.joc(BoardPath("A-BOARD"), lifetime)
  private val bBoard = Board.joc(BoardPath("B-BOARD"), lifetime)

  final class TestJob extends SemaphoreJob(TestJob)
  private object TestJob extends SemaphoreJob.Companion[TestJob]
}
