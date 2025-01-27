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
import js7.data.board.{BoardPath, BoardState, GlobalBoard, GlobalNoticeKey, NoticeKey, NoticePlace, PlannedNoticeKey}
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, CancelOrders, ControlWorkflow, DeleteNotice, PostNotice, ResumeOrder}
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelled, OrderCaught, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderNoticePosted, OrderNoticesConsumed, OrderNoticesConsumptionStarted, OrderNoticesExpected, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderPromptAnswered, OrderPrompted, OrderRetrying, OrderStarted, OrderStateReset, OrderStdoutWritten, OrderStopped, OrderSuspended, OrderTerminated, OrderTransferred}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.plan.PlanId
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
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.collection.View
import scala.concurrent.duration.*
import scala.language.implicitConversions

final class ConsumeNoticesTest
  extends OurTestSuite, ControllerAgentForScalaTest, TransferOrdersWaitingForNoticeTest:

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
    val workflow = Workflow(
      WorkflowPath("CONSUMING-SINGLE"),
      Seq:
        ConsumeNotices(boardPathExpr(s"'${aBoard.path.string}' || '${bBoard.path.string}'")):
          TestJob.execute(agentPath))

    withItem(workflow): workflow =>
      val qualifier = qualifiers.next()
      val globalNoticeKey = GlobalNoticeKey(qualifier)

      TestJob.reset()
      val orderId = OrderId(s"#$qualifier#CONSUMING-NOTICE")
      controller.api
        .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.awaitNextKey[OrderNoticesExpected](orderId)

      execCmd:
        PostNotice(aBoard.path / globalNoticeKey)

      eventWatch.awaitNextKey[OrderNoticesConsumptionStarted](orderId)

      TestJob.continue()
      eventWatch.awaitNextKey[OrderNoticesConsumed](orderId)

      assert(!controllerState.keyTo(BoardState)(aBoard.path).toNoticePlace.contains(globalNoticeKey))
      assert(!controllerState.keyTo(BoardState)(bBoard.path).toNoticePlace.contains(globalNoticeKey))

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderNoticesExpected(Vector(
          aBoard.path \ globalNoticeKey.noticeKey,
          bBoard.path \ globalNoticeKey.noticeKey)),
        OrderNoticesConsumptionStarted(Vector(
          aBoard.path \ globalNoticeKey.noticeKey)),
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
      postOrderToNoticeKey = orderIdToNoticeId,
      expectOrderToNoticeKey = orderIdToNoticeId,
      endOfLife = expr(s"$$js7EpochMilli + ${lifetime.toMillis}"))

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
      assert(events.map(_.value).map:
        case e: OrderNoticePosted => e.copy(endOfLife = endOfLife.some)
        case o => o
      == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderNoticePosted(aBoard.path \ "2022-10-23", endOfLife.some),
        OrderNoticePosted(myBoard.path \ "2022-10-23-X", endOfLife.some),
        OrderMoved(Position(1)),
        OrderNoticesConsumptionStarted(Vector(
          aBoard.path \ "2022-10-23",
          myBoard.path \ "2022-10-23-X")),
        OrderNoticesConsumed(),
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
      val noticeKey = NoticeKey("2022-10-24")
      val orderId = OrderId(s"#${noticeKey.string}#")
      controller.addOrderBlocking(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      eventWatch.awaitNextKey[OrderPrompted](orderId)

      execCmd:
        DeleteNotice(PlanId.Global / aBoard.path / noticeKey)

      execCmd:
        AnswerOrderPrompt(orderId)
      eventWatch.awaitNextKey[OrderTerminated](orderId)

      val events = eventWatch.eventsByKey[OrderEvent](orderId, after = eventId)
      val endOfLife = Timestamp.Epoch
      assert(events.map {
        case e: OrderNoticePosted => e.copy(endOfLife = endOfLife.some)
        case o => o
      } == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderNoticePosted(aBoard.path \ "2022-10-24", endOfLife.some),
        OrderMoved(Position(1)),
        OrderNoticesConsumptionStarted(Vector(
          aBoard.path \ "2022-10-24")),
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
    val noticeKey = NoticeKey(qualifier)

    TestJob.reset()
    val orderId = OrderId(s"#$qualifier#POST-UNUSED")
    controller.api
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    eventWatch.awaitNextKey[OrderNoticesExpected](orderId)

    execCmd:
      PostNotice(PlanId.Global / aBoard.path / noticeKey)

    eventWatch.awaitNextKey[OrderNoticesConsumptionStarted](orderId)

    assert(controllerState.keyTo(BoardState)(aBoard.path).toNoticePlace(PlanId.Global / noticeKey).notice.isDefined)
    assert(controllerState.keyTo(BoardState)(aBoard.path).toNoticePlace(PlanId.Global / noticeKey).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).toNoticePlace(PlanId.Global / noticeKey).consumptionCount == 1)
    assert(!controllerState.keyTo(BoardState)(bBoard.path).toNoticePlace.contains(PlanId.Global / noticeKey))

    execCmd:
      PostNotice(PlanId.Global / bBoard.path / noticeKey)

    TestJob.continue()
    eventWatch.awaitNextKey[OrderNoticesConsumed](orderId)

    // Notice at board has been deleted:
    assert(!controllerState.keyTo(BoardState)(aBoard.path).toNoticePlace.contains(PlanId.Global / noticeKey))

    // Notices posted at board2 while ConsumeNotices still exists:
    assert(controllerState.keyTo(BoardState)(bBoard.path).toNoticePlace(PlanId.Global / noticeKey).notice.isDefined)
    assert(!controllerState.keyTo(BoardState)(bBoard.path).toNoticePlace(PlanId.Global / noticeKey).isInConsumption)
    assert(controllerState.keyTo(BoardState)(bBoard.path).toNoticePlace(PlanId.Global / noticeKey).consumptionCount == 0)

    deleteItems(workflow.path)

    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(
        aBoard.path \ noticeKey,
        bBoard.path \ noticeKey)),
      OrderNoticesConsumptionStarted(Vector(
        aBoard.path \ noticeKey)),
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
    val noticeKey = NoticeKey(qualifier)

    execCmd:
      PostNotice(PlanId.Global / aBoard.path / noticeKey)

    TestJob.reset()
    TestJob.continue()
    val orderId = OrderId(s"#$qualifier#CONSUMING")
    controller.api
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    eventWatch.awaitNextKey[OrderNoticesConsumptionStarted](orderId)

    execCmd:
      PostNotice(PlanId.Global / aBoard.path / noticeKey)
    eventWatch.awaitNextKey[OrderNoticesConsumed](orderId)

    // The secondly posted Notice still exists:
    assert(controllerState.keyTo(BoardState)(aBoard.path).toNoticePlace(PlanId.Global / noticeKey).notice.isDefined)
    assert(!controllerState.keyTo(BoardState)(aBoard.path).toNoticePlace(PlanId.Global / noticeKey).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).toNoticePlace(PlanId.Global / noticeKey).consumptionCount == 0)

    deleteItems(workflow.path)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesConsumptionStarted(Vector(
        aBoard.path \ noticeKey)),
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
    val noticeKey = NoticeKey(qualifier)

    TestJob.reset()
    val aOrderId = OrderId(s"#$qualifier#CONSUMING-A")
    val bOrderId = OrderId(s"#$qualifier#CONSUMING-B")

    execCmd:
      PostNotice(PlanId.Global / aBoard.path / noticeKey)

    for orderId <- View(aOrderId, bOrderId) do
      controller.api
        .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.awaitNextKey[OrderNoticesConsumptionStarted](orderId)

    assert(controllerState.keyTo(BoardState)(aBoard.path).toNoticePlace(PlanId.Global / noticeKey).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).toNoticePlace(PlanId.Global / noticeKey).consumptionCount == 2)

    TestJob.continue()
    eventWatch.await[OrderNoticesConsumed](_.key == aOrderId)
    assert(controllerState.keyTo(BoardState)(aBoard.path).toNoticePlace(PlanId.Global / noticeKey).isInConsumption)
    assert(controllerState.keyTo(BoardState)(aBoard.path).toNoticePlace(PlanId.Global / noticeKey).consumptionCount == 1)

    TestJob.continue()
    eventWatch.await[OrderNoticesConsumed](_.key == aOrderId)
    awaitAndAssert(
      controllerState.keyTo(BoardState)(aBoard.path).toNoticePlace.get(PlanId.Global / noticeKey).isEmpty)

    deleteItems(workflow.path)
    assert(eventWatch.eventsByKey[OrderEvent](aOrderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesConsumptionStarted(Vector(
        aBoard.path \ noticeKey)),
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
        aBoard.path \ noticeKey)),
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
    val noticeId = PlanId.Global / aBoard.path / qualifier

    TestJob.reset()
    val orderId = OrderId(s"#$qualifier#NESTED")
    controller.api
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow
    eventWatch.awaitNextKey[OrderNoticesExpected](orderId)

    execCmd:
      PostNotice(noticeId)

    eventWatch.awaitNextKey[OrderStdoutWritten](orderId)


    assert(controllerState.toNoticePlace.contains(noticeId))
    assert(controllerState.toNoticePlace(noticeId).isInConsumption)
    assert(controllerState.toNoticePlace(noticeId).consumptionCount == 2)
    assert(!controllerState.keyTo(BoardState)(bBoard.path).toNoticePlace.contains(noticeId.plannedNoticeKey))

    TestJob.continue()
    eventWatch.awaitNextKey[OrderPrompted](orderId)
    assert(controllerState.toNoticePlace(noticeId).notice.isDefined)
    assert(controllerState.toNoticePlace(noticeId).isInConsumption)
    assert(controllerState.toNoticePlace(noticeId).consumptionCount == 1)

    execCmd:
      AnswerOrderPrompt(orderId)
    eventWatch.awaitNextKey[OrderNoticesConsumed](orderId)

    deleteItems(workflow.path)

    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(
        noticeId.boardPath \ noticeId.noticeKey)),
      OrderNoticesConsumptionStarted(Vector(
        noticeId.boardPath \ noticeId.noticeKey)),
      OrderNoticesConsumptionStarted(Vector(
        noticeId.boardPath \ noticeId.noticeKey)),
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
    val noticeId = PlanId.Global / aBoard.path / qualifier

    TestJob.reset()
    val orderId = OrderId(s"#$qualifier#CONSUMING-FAILING")
    controller.api
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow

    execCmd:
      PostNotice(noticeId)

    eventWatch.awaitNextKey[OrderNoticesConsumptionStarted](orderId)

    TestJob.continue()
    eventWatch.awaitNextKey[OrderNoticesConsumed](orderId)
    eventWatch.awaitNextKey[OrderFailed](orderId)

    assert(controllerState.toNoticePlace(noticeId).notice.isDefined)
    assert(!controllerState.toNoticePlace(noticeId).isInConsumption)
    assert(controllerState.toNoticePlace(noticeId).consumptionCount == 0)

    execCmd(CancelOrders(Seq(orderId)))
    deleteItems(workflow.path)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(
        noticeId.boardPath \ noticeId.noticeKey)),
      OrderNoticesConsumptionStarted(Vector(
        noticeId.boardPath \ noticeId.noticeKey)),
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
    val noticeId = PlanId.Global / aBoard.path / qualifier

    val orderId = OrderId(s"#$qualifier#CANCEL-WHILE-PROMTING")
    controller.api
      .addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
      .await(99.s).orThrow

    execCmd:
      PostNotice(noticeId)

    eventWatch.awaitNextKey[OrderPrompted](orderId)
    execCmd:
      CancelOrders(Seq(orderId))

    eventWatch.awaitNextKey[OrderNoticesConsumed](orderId)
    eventWatch.awaitNextKey[OrderCancelled](orderId)

    assert(controllerState.toNoticePlace(noticeId).notice.isDefined)
    assert(!controllerState.toNoticePlace(noticeId).isInConsumption)
    assert(controllerState.toNoticePlace(noticeId).consumptionCount == 0)

    deleteItems(workflow.path)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflow.id, deleteWhenTerminated = true),
      OrderStarted,
      OrderNoticesExpected(Vector(
        noticeId.boardPath \ noticeId.noticeKey)),
      OrderNoticesConsumptionStarted(Vector(
        noticeId.boardPath \ noticeId.noticeKey)),
      OrderPrompted(StringValue("PROMPT")),
      OrderStateReset,
      OrderNoticesConsumed(true),
      OrderCancelled,
      OrderDeleted))

  "TransferOrders of Order.ExpectingNotice" in:
    val eventId = eventWatch.lastAddedEventId
    testTransferOrders(
      boardPath => ConsumeNotices(ExpectNotice(boardPath), subworkflow = Workflow.empty),
      (board1, board2, workflowId1, workflowId2, orderId, noticeKey) =>
        assert(eventWatch.eventsByKey[OrderEvent](orderId, eventId) == Seq(
          OrderAdded(workflowId1, deleteWhenTerminated = true),
          OrderStarted,
          OrderNoticesExpected(Vector(
            board1.path \ noticeKey)),
          OrderStateReset,
          OrderTransferred(workflowId2 /: Position(0)),
          OrderNoticesExpected(Vector(
            board2.path \ noticeKey)),
          OrderNoticesConsumptionStarted(Vector(
            board2.path \ noticeKey)),
          OrderNoticesConsumed(),
          OrderFinished(),
          OrderDeleted))

        assert(controllerState.keyTo(BoardState)(board2.path) ==
          BoardState(board2, toNoticePlace = Map.empty)))

  "JS-2015 ConsumeOrders in Try/Retry with 0s delay" in:
    val noticeId = PlanId.Global / aBoard.path / "2022-10-25"
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
        case e: OrderNoticePosted => e.copy(endOfLife = endOfLife.some)
        case o => o
      } == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderNoticePosted(noticeId, endOfLife.some),
        OrderMoved(Position(1) / "try+0" % 0),

        OrderNoticesConsumptionStarted(Vector(
          noticeId.boardPath \ noticeId.noticeKey)),
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
          noticeId.boardPath \ noticeId.noticeKey)),
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
      eventWatch.awaitNextKey[OrderSuspended](orderId)

      val checked = controller.api
        .executeCommand(
          ResumeOrder(orderId, position = Some(Position(0) / BranchId.ConsumeNotices % 0)))
        .await(99.s)
      assert(checked == Left(UnreachableOrderPositionProblem))

      execCmd(CancelOrders(Seq(orderId)))
      eventWatch.awaitNextKey[OrderDeleted](orderId)

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
      eventWatch.awaitNextKey[OrderStopped](orderId)

      execCmd(CancelOrders(Seq(orderId)))
      eventWatch.awaitNextKey[OrderCancelled](orderId)

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
      eventWatch.awaitNextKey[OrderFailed](orderId)

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
        OrderNoticePosted(board.path \ "2024-12-11", endOfLife = None),
        OrderMoved(Position(1)),
        OrderNoticesConsumptionStarted(Vector:
          board.path \ "2024-12-11"),

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
