package js7.tests.notice

import cats.effect.unsafe.IORuntime
import cats.syntax.option.*
import java.time.LocalDate
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.board.BoardPathExpression.ExpectNotice
import js7.data.board.BoardPathExpression.syntax.boardPathToExpr
import js7.data.board.BoardPathExpressionParser.boardPathExpr
import js7.data.board.{BoardPath, BoardState, GlobalBoard, Notice, NoticeId, NoticePlace}
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, CancelOrders, ControlWorkflow, DeleteNotice, PostNotice, ResumeOrder}
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.OrderNoticesConsumptionStarted.Consumption
import js7.data.order.OrderEvent.OrderNoticesExpected.Expected
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelled, OrderCaught, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderNoticePosted, OrderNoticesConsumed, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderPromptAnswered, OrderPrompted, OrderRetrying, OrderStarted, OrderStateReset, OrderStdoutWritten, OrderStopped, OrderSuspended, OrderTerminated, OrderTransferred}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.problems.UnreachableOrderPositionProblem
import js7.data.value.StringValue
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{ConsumeNotices, Execute, Fail, If, Options, PostNotices, Prompt, Retry, TryInstruction}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Instruction, Workflow, WorkflowPath}
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.jobs.{EmptyJob, FailingJob, SemaphoreJob}
import js7.tests.notice.ConsumeNoticesTest.*
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import scala.collection.View
import scala.concurrent.duration.*
import scala.language.implicitConversions

final class ConsumeNoticesTest
  extends OurTestSuite, ControllerAgentForScalaTest, BlockingItemUpdater, TransferOrdersWaitingForNoticeTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 1ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected def agentPaths = Seq(agentPath, bAgentPath)
  protected def items = Seq(aBoard, bBoard)

  private val qualifiers = for i <- Iterator.from(0) yield
    LocalDate.of(3333, 1, 1).plusDays(i).toString

  "A single Order" in:
    // board2 is referenced but not required to have a Notice
    val workflow = updateItem:
      Workflow(WorkflowPath("CONSUMING-SINGLE"), Seq(
        ConsumeNotices(boardPathExpr(s"'${aBoard.path.string}' || '${bBoard.path.string}'")):
          TestJob.execute(agentPath)))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    TestJob.reset()
    val orderId = OrderId(s"#$qualifier#CONSUMING")
    controller.api
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticesExpected](_.key == orderId)

    execCmd:
      PostNotice(aBoard.path, noticeId)

    eventWatch.await[OrderNoticesConsumptionStarted](_.key == orderId)

    TestJob.continue()
    eventWatch.await[OrderNoticesConsumed](_.key == orderId)

    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice.get(noticeId).isEmpty)
    assert(controllerState.keyTo(BoardState)(bBoard.path).idToNotice.get(noticeId).isEmpty)

    deleteItems(workflow.path)

    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(
        Expected(aBoard.path, noticeId),
        Expected(bBoard.path, noticeId))),
      OrderNoticesConsumptionStarted(Vector(
        Consumption(aBoard.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 1),
      OrderDetachable, OrderDetached,
      OrderNoticesConsumed(),
      OrderFinished(),
      OrderDeleted))

  "Simple test with two boards" in:
    val orderIdToNoticeId = expr(
      """match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#(.*)', '$1-$2')""")

    val myBoard = GlobalBoard(
      BoardPath("MY-BOARD"),
      postOrderToNoticeId = orderIdToNoticeId,
      endOfLife = expr(s"$$js7EpochMilli + ${lifetime.toMillis}"),
      expectOrderToNoticeId = orderIdToNoticeId)

    val workflow = Workflow(
      WorkflowPath("SIMPLE-WITH-TWO-BOARDS"), Seq(
        PostNotices(Seq(aBoard.path, myBoard.path)),
        ConsumeNotices(
          boardPathExpr(s"'${aBoard.path.string}' && '${myBoard.path.string}'"),
          subworkflow = Workflow.empty)))

    withItems((myBoard, workflow)): (_, workflow) =>
      val orderId = OrderId("#2022-10-23#X")
      val events = controller.runOrder(
        FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      val endOfLife = Timestamp.Epoch
      assert(events.map(_.value).map {
        case e: OrderNoticePosted => e.copy(notice = e.notice.copy(endOfLife = endOfLife.some))
        case o => o
      } == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderNoticePosted(Notice(NoticeId("2022-10-23"), aBoard.path, endOfLife.some)),
        OrderNoticePosted(Notice(NoticeId("2022-10-23-X"), myBoard.path, endOfLife.some)),
        OrderMoved(Position(1)),
        OrderNoticesConsumptionStarted(Vector(
          Consumption(aBoard.path, NoticeId("2022-10-23")),
          Consumption(myBoard.path, NoticeId("2022-10-23-X")))),
        OrderNoticesConsumed(false),
        OrderFinished(),
        OrderDeleted))

  "ConsumeNotices with interfering DeleteNotice command" in:
    val workflow = Workflow(
      WorkflowPath("SIMPLE-WITH-TWO-BOARDS"), Seq(
        PostNotices(Seq(aBoard.path)),
        ConsumeNotices(boardPathExpr(s"'${aBoard.path.string}'")):
          Prompt(expr("'PROMPT'"))))

    withItem(workflow): workflow =>
      val eventId = eventWatch.lastAddedEventId
      val noticeId = NoticeId("2022-10-24")
      val orderId = OrderId(s"#${noticeId.noticeKey.string}#")
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      eventWatch.await[OrderPrompted](_.key == orderId)

      execCmd:
        DeleteNotice(aBoard.path, noticeId)

      execCmd:
        AnswerOrderPrompt(orderId)
      eventWatch.await[OrderTerminated](_.key == orderId)

      val events = eventWatch.eventsByKey[OrderEvent](orderId, after = eventId)
      val endOfLife = Timestamp.Epoch
      assert(events.map {
        case e: OrderNoticePosted => e.copy(notice = e.notice.copy(endOfLife = endOfLife.some))
        case o => o
      } == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderNoticePosted(Notice(NoticeId("2022-10-24"), aBoard.path, endOfLife.some)),
        OrderMoved(Position(1)),
        OrderNoticesConsumptionStarted(Vector(
          Consumption(aBoard.path, NoticeId("2022-10-24")))),
        OrderPrompted(StringValue("PROMPT")),
        OrderPromptAnswered(),
        OrderMoved(Position(1) / "consumeNotices" % 1),
        OrderNoticesConsumed(false),
        OrderFinished(),
        OrderDeleted))

  "A single Order waiting for one of two Notices, posting one Notice, then the other" in:
    val workflow = updateItem:
      Workflow(WorkflowPath("CONSUMING-SINGLE"), Seq(
        ConsumeNotices(boardPathExpr(s"'${aBoard.path.string}' || '${bBoard.path.string}'")):
          TestJob.execute(agentPath)))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    TestJob.reset()
    val orderId = OrderId(s"#$qualifier#POST-UNUSED")
    controller.api
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticesExpected](_.key == orderId)

    execCmd:
      PostNotice(aBoard.path, noticeId)

    eventWatch.await[OrderNoticesConsumptionStarted](_.key == orderId)

    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).notice.isDefined)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).consumptionCount == 1)
    assert(!controllerState.keyTo(BoardState)(bBoard.path).idToNotice.contains(noticeId))

    execCmd:
      PostNotice(bBoard.path, noticeId)

    TestJob.continue()
    eventWatch.await[OrderNoticesConsumed](_.key == orderId)

    // Notice at board has been deleted:
    assert(!controllerState.keyTo(BoardState)(aBoard.path).idToNotice.contains(noticeId))

    // Notices posted at board2 while ConsumeNotices still exists:
    assert(controllerState.keyTo(BoardState)(bBoard.path).idToNotice(noticeId).notice.isDefined)
    assert(!controllerState.keyTo(BoardState)(bBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(bBoard.path).idToNotice(noticeId).consumptionCount == 0)

    deleteItems(workflow.path)

    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(
        Expected(aBoard.path, noticeId),
        Expected(bBoard.path, noticeId))),
      OrderNoticesConsumptionStarted(Vector(
        Consumption(aBoard.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 1),
      OrderDetachable, OrderDetached,
      OrderNoticesConsumed(),
      OrderFinished(),
      OrderDeleted))

  "PostNotice while consuming an earlier notice" in:
    val workflow = updateItem:
      Workflow(WorkflowPath("POST-WHILE-CONSUMING"), Seq(
        ConsumeNotices(boardPathExpr(s"'${aBoard.path.string}'")):
          TestJob.execute(agentPath)))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    execCmd:
      PostNotice(aBoard.path, noticeId)

    TestJob.reset()
    TestJob.continue()
    val orderId = OrderId(s"#$qualifier#CONSUMING")
    controller.api
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticesConsumptionStarted](_.key == orderId)

    execCmd:
      PostNotice(aBoard.path, noticeId)
    eventWatch.await[OrderNoticesConsumed](_.key == orderId)

    // The secondly posted Notice still exists:
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).notice.isDefined)
    assert(!controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).consumptionCount == 0)

    deleteItems(workflow.path)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesConsumptionStarted(Vector(
        Consumption(aBoard.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 1),
      OrderDetachable,
      OrderDetached,
      OrderNoticesConsumed(),
      OrderFinished(),
      OrderDeleted))

  "Two concurrent ConsumeNotices" in:
    val workflow = updateItem:
      Workflow(WorkflowPath("CONSUMING-TWO-ORDERS"), Seq(
        ConsumeNotices(boardPathExpr(s"'${aBoard.path.string}'")):
          TestJob.execute(agentPath)))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    TestJob.reset()
    val aOrderId = OrderId(s"#$qualifier#CONSUMING-A")
    val bOrderId = OrderId(s"#$qualifier#CONSUMING-B")

    execCmd:
      PostNotice(aBoard.path, noticeId)

    for orderId <- View(aOrderId, bOrderId) do
      controller.api
        .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.await[OrderNoticesConsumptionStarted](_.key == orderId)

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
      OrderNoticesConsumptionStarted(Vector(
        Consumption(aBoard.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 1),
      OrderDetachable,
      OrderDetached,
      OrderNoticesConsumed(),
      OrderFinished(),
      OrderDeleted))

    assert(eventWatch.eventsByKey[OrderEvent](bOrderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesConsumptionStarted(Vector(
        Consumption(aBoard.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 1),
      OrderDetachable,
      OrderDetached,
      OrderNoticesConsumed(),
      OrderFinished(),
      OrderDeleted))

  "Nested ConsumeNotcies" in:
    val workflow = updateItem:
      Workflow(
        WorkflowPath("CONSUMING-NESTED"),
        Seq(
          ConsumeNotices(
            boardPathExpr(s"'${aBoard.path.string}'"),
            subworkflow = Workflow.of(
              ConsumeNotices(boardPathExpr(s"'${aBoard.path.string}' || '${bBoard.path.string}'")):
                Execute(WorkflowJob.Name("JOB")),
              Prompt(expr("'PROMPT'"))))),
        nameToJob = Map(
          WorkflowJob.Name("JOB") -> TestJob.workflowJob(agentPath)))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    TestJob.reset()
    val orderId = OrderId(s"#$qualifier#NESTED")
    controller.api
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    eventWatch.await[OrderNoticesExpected](_.key == orderId)

    execCmd:
      PostNotice(aBoard.path, noticeId)

    eventWatch.await[OrderStdoutWritten](_.key == orderId)

    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).notice.isDefined)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).consumptionCount == 2)
    assert(!controllerState.keyTo(BoardState)(bBoard.path).idToNotice.contains(noticeId))

    TestJob.continue()
    eventWatch.await[OrderPrompted](_.key == orderId)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).notice.isDefined)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).consumptionCount == 1)

    execCmd:
      AnswerOrderPrompt(orderId)
    eventWatch.await[OrderNoticesConsumed](_.key == orderId)

    deleteItems(workflow.path)

    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(
        Expected(aBoard.path, noticeId))),
      OrderNoticesConsumptionStarted(Vector(
        Consumption(aBoard.path, noticeId))),
      OrderNoticesConsumptionStarted(Vector(
        Consumption(aBoard.path, noticeId))),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted(subagentId),
      OrderStdoutWritten("TestJob\n"),
      OrderProcessed(OrderOutcome.succeeded),
      OrderMoved(Position(0) / "consumeNotices" % 0 / "consumeNotices" % 1),
      OrderDetachable, OrderDetached,
      OrderNoticesConsumed(),
      OrderPrompted(StringValue("PROMPT")),
      OrderPromptAnswered(),
      OrderMoved(Position(0) / "consumeNotices" % 2),
      OrderNoticesConsumed(),
      OrderFinished(),
      OrderDeleted))

  "Failing ConsumeNotices block does not consume the Notice" in:
    val workflow = updateItem:
      Workflow(WorkflowPath("CONSUMING-FAILING"), Seq(
        ConsumeNotices(boardPathExpr(s"'${aBoard.path.string}'")):
          Fail()))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    TestJob.reset()
    val orderId = OrderId(s"#$qualifier#CONSUMING-FAILING")
    controller.api
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow

    execCmd:
      PostNotice(aBoard.path, noticeId)

    eventWatch.await[OrderNoticesConsumptionStarted](_.key == orderId)

    TestJob.continue()
    eventWatch.await[OrderNoticesConsumed](_.key == orderId)
    eventWatch.await[OrderFailed](_.key == orderId)

    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).notice.isDefined)
    assert(!controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).consumptionCount == 0)

    execCmd(CancelOrders(Seq(orderId)))
    deleteItems(workflow.path)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(
        Expected(aBoard.path, noticeId))),
      OrderNoticesConsumptionStarted(Vector(
        Consumption(aBoard.path, noticeId))),
      OrderOutcomeAdded(OrderOutcome.failed),
      OrderNoticesConsumed(true),
      OrderFailed(Position(0)),
      OrderCancelled,
      OrderDeleted))

  "Cancel while consuming a Notice and sticking in Promting" in:
    val workflow = updateItem:
      Workflow(WorkflowPath("CANCEL-WHILE-PROMPTING"), Seq(
        ConsumeNotices(boardPathExpr(s"'${aBoard.path.string}'")):
          Prompt(expr("'PROMPT'"))))

    val qualifier = qualifiers.next()
    val noticeId = NoticeId(qualifier)

    val orderId = OrderId(s"#$qualifier#CANCEL-WHILE-PROMTING")
    controller.api
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow

    execCmd:
      PostNotice(aBoard.path, noticeId)

    eventWatch.await[OrderPrompted](_.key == orderId)
    execCmd:
      CancelOrders(Seq(orderId))

    eventWatch.await[OrderNoticesConsumed](_.key == orderId)
    eventWatch.await[OrderCancelled](_.key == orderId)

    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).notice.isDefined)
    assert(!controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).idToNotice(noticeId).consumptionCount == 0)

    deleteItems(workflow.path)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(
        Expected(aBoard.path, noticeId))),
      OrderNoticesConsumptionStarted(Vector(
        Consumption(aBoard.path, noticeId))),
      OrderPrompted(StringValue("PROMPT")),
      OrderStateReset,
      OrderNoticesConsumed(true),
      OrderCancelled,
      OrderDeleted))

  "TransferOrders of Order.ExpectingNotice" in:
    val eventId = eventWatch.lastAddedEventId
    testTransferOrders(
      boardPath => ConsumeNotices(ExpectNotice(boardPath), subworkflow = Workflow.empty),
      (board1, board2, workflowId1, workflowId2, orderId, noticeId) =>
        assert(eventWatch.eventsByKey[OrderEvent](orderId, eventId) == Seq(
          OrderAdded(workflowId1, deleteWhenTerminated = true),
          OrderStarted,
          OrderNoticesExpected(Vector(
            Expected(board1.path, noticeId))),
          OrderStateReset,
          OrderTransferred(workflowId2 /: Position(0)),
          OrderNoticesExpected(Vector(
            Expected(board2.path, noticeId))),
          OrderNoticesConsumptionStarted(Vector(
            Consumption(board2.path, noticeId))),
          OrderNoticesConsumed(),
          OrderFinished(),
          OrderDeleted))

        assert(controllerState.keyTo(BoardState)(board2.path) ==
          BoardState(board2, idToNotice = Map.empty)))

  "JS-2015 ConsumeOrders in Try/Retry with 0s delay" in:
    val noticeId = NoticeId("2022-10-25")
    val workflow = Workflow(
      WorkflowPath("CONSUME-NOTICES-IN-RETRY"),
      Seq(
        PostNotices(Seq(aBoard.path)),
        TryInstruction(
          Workflow.of(
            ConsumeNotices(boardPathExpr(s"'${aBoard.path.string}'")):
              FailingJob.execute(agentPath)),
          Workflow.of(
            Retry()),
          retryDelays = Some(Vector(0.s)),
          maxTries = Some(2))))

    withItem(workflow, awaitDeletion = true): workflow =>
      val orderId = OrderId("#2022-10-25#")
      val events = controller.runOrder:
        FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
      val endOfLife = Timestamp.Epoch
      assert(events.map(_.value).map {
        case e: OrderNoticePosted => e.copy(notice = e.notice.copy(endOfLife = endOfLife.some))
        case o => o
      } == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderNoticePosted(Notice(noticeId, aBoard.path, endOfLife.some)),
        OrderMoved(Position(1) / "try+0" % 0),

        OrderNoticesConsumptionStarted(Vector(
          Consumption(aBoard.path, noticeId))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.Failed(Some("💥FailingJob failed💥"))),
        OrderDetachable,
        OrderDetached,
        OrderNoticesConsumed(failed = true),

        OrderCaught(Position(1) / "catch+0" % 0),
        OrderRetrying(),
        OrderMoved(Position(1) / "try+1" % 0),

        OrderNoticesConsumptionStarted(Vector(
          Consumption(aBoard.path, noticeId))),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.Failed(Some("💥FailingJob failed💥"))),
        OrderDetachable,
        OrderDetached,
        OrderNoticesConsumed(failed = true),

        OrderFailed(Position(1) / "try+1" % 0)))
      execCmd:
        CancelOrders(Seq(orderId))

  "ResumeOrder into ConsumeNotices block is rejected (JS-2121)" in:
    val workflow = Workflow(
      WorkflowPath("RESUME-INTO-CONSUME-NOTICES"),
      Seq(
        ConsumeNotices(boardPathExpr(s"'${aBoard.path.string}'")):
          EmptyJob.execute(agentPath)))

    withItem(workflow): workflow =>
      execCmd:
        ControlWorkflow(workflow.id, addBreakpoints = Set(Position(0)))

      val orderId = OrderId("#2024-03-19#")
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      eventWatch.await[OrderSuspended](_.key == orderId)

      val checked = controller.api
        .executeCommand(
          ResumeOrder(orderId, position = Some(Position(0) / BranchId.ConsumeNotices % 0)))
        .await(99.s)
      assert(checked == Left(UnreachableOrderPositionProblem))

      execCmd(CancelOrders(Seq(orderId)))
      eventWatch.await[OrderDeleted](_.key == orderId)

  "JS-2124 FIX 'ConsumeNotices' Instruction is expected at position, with Options" in:
    val workflow = Workflow(
      WorkflowPath("OPTION-CONSUME-NOTICE-IF-FAIL"),
      Seq:
        Options(stopOnFailure = true)(
          PostNotices(Seq(aBoard.path)),
          ConsumeNotices(boardPathExpr(s"'${aBoard.path.string}'")):
            If(expr("true")):
              Fail()))

    withItem(workflow): workflow =>
      val orderId = OrderId("#2024-03-20#Options-ConsumeNotices-If-Fail")
      controller.api
        .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.await[OrderStopped](_.key == orderId)

      execCmd(CancelOrders(Seq(orderId)))
      eventWatch.await[OrderCancelled](_.key == orderId)

  "JS-2124 FIX 'ConsumeNotices' Instruction is expected at position, without Options" in:
    val workflow = Workflow(
      WorkflowPath("CONSUME-NOTICE-IF-FAIL"),
      Seq(
        PostNotices(Seq(aBoard.path)),
        ConsumeNotices(boardPathExpr(s"'${aBoard.path.string}'")):
          If(expr("true")):
            Fail()))

    withItem(workflow): workflow =>
      val orderId = OrderId("#2024-03-20#ConsumeNotices-If-Fail")
      controller.api
        .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.await[OrderFailed](_.key == orderId)

  "JS-2186 ConsumeNotices block fails with jobs on different Agents" in:
    val board = GlobalBoard.joc(BoardPath("BOARD"), lifetime = None)
    val aJobName = WorkflowJob.Name("A-JOB")
    val bJobName = WorkflowJob.Name("B-JOB")
    val workflow = Workflow(
      WorkflowPath("CONSUMENOTICES"),
      Seq(
        PostNotices(board.path :: Nil),
        ConsumeNotices(board.path)(
          Execute.Named(aJobName),
          Execute.Named(bJobName))),
      nameToJob = Map(
        aJobName -> WorkflowJob(agentPath, ShellScriptExecutable(":")),
        bJobName -> WorkflowJob(bAgentPath, ShellScriptExecutable(":"))))

    withItems((board, workflow)): (board, workflow) =>
      val events = controller.runOrder:
        FreshOrder(OrderId("#2024-12-11#"), workflow.path, deleteWhenTerminated = true)
      assert(events.map(_.value) == Vector(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderNoticePosted(Notice(NoticeId("2024-12-11"), board.path, endOfLife = None)),
        OrderMoved(Position(1)),
        OrderNoticesConsumptionStarted(Vector:
          OrderNoticesConsumptionStarted.Consumption(board.path, NoticeId("2024-12-11"))),

        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeededRC0),
        OrderMoved(Position(1) / BranchId.ConsumeNotices % 1),
        OrderDetachable,
        OrderDetached,

        OrderAttachable(bAgentPath),
        OrderAttached(bAgentPath),
        OrderProcessingStarted(bSubagentId),
        OrderProcessed(OrderOutcome.succeededRC0),
        OrderMoved(Position(1) / BranchId.ConsumeNotices % 2),
        OrderDetachable,
        OrderDetached,

        OrderNoticesConsumed(),
        OrderFinished(),
        OrderDeleted))


object ConsumeNoticesTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)

  private val bAgentPath = AgentPath("B-AGENT")
  private val bSubagentId = toLocalSubagentId(bAgentPath)

  // One lifetime per board
  private val lifetime = 1.day
  private val aBoard = GlobalBoard.joc(BoardPath("A-BOARD"), Some(lifetime))
  private val bBoard = GlobalBoard.joc(BoardPath("B-BOARD"), Some(lifetime))

  final class TestJob extends SemaphoreJob(TestJob)
  private object TestJob extends SemaphoreJob.Companion[TestJob]
