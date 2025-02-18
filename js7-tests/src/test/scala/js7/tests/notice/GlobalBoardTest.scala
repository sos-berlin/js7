package js7.tests.notice

import cats.effect.unsafe.IORuntime
import cats.syntax.option.*
import fs2.Stream
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problem
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.time.{TestAlarmClock, Timestamp}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.controller.RunningController
import js7.data.Problems.ItemIsStillReferencedProblem
import js7.data.agent.AgentPath
import js7.data.board.BoardPathExpression.ExpectNotice
import js7.data.board.BoardPathExpression.syntax.{&, boardPathToExpr}
import js7.data.board.NoticeEvent.NoticeDeleted
import js7.data.board.{BoardPath, BoardPathExpression, BoardState, GlobalBoard, GlobalNoticeKey, Notice, NoticeKey, NoticePlace, PlannedBoard}
import js7.data.controller.ControllerCommand
import js7.data.controller.ControllerCommand.{CancelOrders, DeleteNotice, PostNotice, ResumeOrder, SuspendOrders}
import js7.data.item.ItemOperation.{AddVersion, DeleteSimple, RemoveVersioned}
import js7.data.item.{ItemRevision, VersionId}
import js7.data.order.Order.Fresh
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelled, OrderCoreEvent, OrderDeleted, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderNoticePosted, OrderNoticesExpected, OrderNoticesRead, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStateReset, OrderSuspended, OrderSuspensionMarked, OrderTransferred}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.plan.{Plan, PlanId}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{ExpectNotices, PostNotices, TryInstruction}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.jobs.{EmptyJob, SemaphoreJob}
import js7.tests.notice.GlobalBoardTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.concurrent.duration.*

final class GlobalBoardTest
  extends OurTestSuite, ControllerAgentForScalaTest, TransferOrdersWaitingForNoticeTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 1ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected def agentPaths = Seq(agentPath)

  protected def items = boards ++ Seq(
    posting0Workflow, expecting0Workflow,
    posting12Workflow, expecting012Workflow,
    expecting01Workflow, expecting02Workflow,
    postingAgentWorkflow, expectingAgentWorkflow)

  private val clock = TestAlarmClock(startTimestamp)

  override protected def controllerTestWiring = RunningController.TestWiring(
    alarmClock = Some(clock))

  "ExpectNotices" - {
    "expect(0, 1), post(1, 2), expect(0, 2), post(0)" in:
      val qualifier = nextQualifier()
      val notices = for (boardPath, endOfLife) <- boards.map(_.path) zip endOfLifes yield
        Notice(boardPath / GlobalNoticeKey(qualifier), endOfLife.some)
      val Seq(notice0, notice1, notice2) = notices

      val expecting01OrderIds = Seq(
        OrderId(s"#$qualifier#EXPECTING-0-1-A"),
        OrderId(s"#$qualifier#EXPECTING-0-1-B"))
      controller.api.addOrders(
        Stream.iterable(expecting01OrderIds).map(FreshOrder(_, expecting01Workflow.path))
      ).await(99.s).orThrow
      for orderId <- expecting01OrderIds do eventWatch.await[OrderNoticesExpected](_.key == orderId)

      assert(controllerState.orderToAvailableNotices(expecting01OrderIds(0)).isEmpty)
      assert(controllerState.orderToStillExpectedNotices(expecting01OrderIds(0)).toSet ==
        Set(notice0.id, notice1.id))

      val posting12OrderId = OrderId(s"#$qualifier#POSTING-1-2")
      controller.runOrder(FreshOrder(posting12OrderId, posting12Workflow.path))

      assert(controllerState.orderToAvailableNotices(expecting01OrderIds(0)) == Seq(notice1))
      assert(controllerState.orderToStillExpectedNotices(expecting01OrderIds(0)) == Seq(notice0.id))

      assert(controllerState.toNoticePlace.toMap == Map(
        notice0.id -> NoticePlace(None, expecting01OrderIds.toSet),
        notice1.id -> NoticePlace(Some(notice1), expecting01OrderIds.toSet),
        notice2.id -> NoticePlace(Some(notice2))))

      val posting0OrderId = OrderId(s"#$qualifier#POSTING-0")
      controller.runOrder(FreshOrder(posting0OrderId, posting0Workflow.path))

      // Look at the Global Plan //
      assert:
        controllerState.toPlan.toMap ==
          Map:
            PlanId.Global -> Plan(
              PlanId.Global,
              orderIds = Set(posting0OrderId, posting12OrderId) ++ expecting01OrderIds,
              Seq(
                PlannedBoard(PlanId.Global / board0.path, Map(
                  notice0.noticeKey -> NoticePlace(Some(notice0)))),
                PlannedBoard(PlanId.Global / board1.path, Map(
                  notice1.noticeKey -> NoticePlace(Some(notice1)))),
                PlannedBoard(PlanId.Global / board2.path, Map(
                  notice2.noticeKey -> NoticePlace(Some(notice2))))),
              isClosed = false)

      for orderId <- expecting01OrderIds do
        eventWatch.await[OrderFinished](_.key == orderId)
        val expectingEvents = eventWatch.eventsByKey[OrderCoreEvent](orderId)
        assert(expectingEvents == Seq(
          OrderAdded(expecting01Workflow.id),
          OrderStarted,
          OrderNoticesExpected(Vector(
            notice0.boardPath \ notice0.noticeKey,
            notice1.boardPath \ notice1.noticeKey)),
          OrderNoticesRead,
          OrderMoved(Position(1)),
          OrderFinished()))

      val readerEvents = controller.runOrder:
        FreshOrder(OrderId(s"#$qualifier#EXPECTING-0-2"), expecting02Workflow.path)
      assert(readerEvents.map(_.value) == Seq(
        OrderAdded(expecting02Workflow.id),
        OrderStarted,
        OrderNoticesRead,
        OrderMoved(Position(1)),
        OrderFinished()))

      assert(controllerState.toNoticePlace.toMap == Map(
        notice0.id -> NoticePlace(Some(notice0)),
        notice1.id -> NoticePlace(Some(notice1)),
        notice2.id -> NoticePlace(Some(notice2))))

    "Two orders expect a notice, then post the notice" in:
      val qualifier = nextQualifier()
      val notice = Notice(board0.path \ NoticeKey(qualifier), endOfLife0.some)

      val expectingOrderIds = Seq(
        OrderId(s"#$qualifier#EXPECTING-A"),
        OrderId(s"#$qualifier#EXPECTING-B"))
      controller.api.addOrders:
        Stream.iterable(expectingOrderIds).map(FreshOrder(_, expecting0Workflow.path))
      .await(99.s).orThrow
      for orderId <- expectingOrderIds do eventWatch.await[OrderNoticesExpected](_.key == orderId)

      val posterEvents = controller.runOrder(
        FreshOrder(OrderId(s"#$qualifier#POSTING"), posting0Workflow.path))
      assert(posterEvents.map(_.value) == Seq(
        OrderAdded(posting0Workflow.id),
        OrderStarted,
        OrderNoticePosted(notice.id, notice.endOfLife),
        OrderMoved(Position(1)),
        OrderFinished()))

      assert(controllerState.toNoticePlace.filterKeys(_.boardPath == board0.path).toMap == Map(
        board0.path \ NoticeKey("2222-01-01") -> NoticePlace(
          Some(Notice(board0.path \ NoticeKey("2222-01-01"), endOfLife0.some))), // from previous test
        notice.id -> NoticePlace(Some(notice))))

      for orderId <- expectingOrderIds do
        eventWatch.await[OrderFinished](_.key == orderId)
        val expectingEvents = eventWatch.eventsByKey[OrderCoreEvent](orderId)
        assert(expectingEvents == Seq(
          OrderAdded(expecting0Workflow.id),
          OrderStarted,
          OrderNoticesExpected(Vector:
            notice.id),
          OrderNoticesRead,
          OrderMoved(Position(1)),
          OrderFinished()))

      assert(controllerState.toNoticePlace.filterKeys(_.boardPath == board0.path).toMap == Map(
        board0.path \ NoticeKey("2222-01-01") -> NoticePlace(
          Some(Notice(board0.path \ NoticeKey("2222-01-01"), endOfLife0.some))), // from previous test
        board0.path \ NoticeKey("2222-02-02") -> NoticePlace(
          Some(Notice(board0.path \ NoticeKey("2222-02-02"), endOfLife0.some)))))

      assert(controllerState.toNoticePlace.filterKeys(_.boardPath == board0.path).toMap == Map(
        board0.path \ NoticeKey("2222-01-01") -> NoticePlace(
          Some(Notice(board0.path \ NoticeKey("2222-01-01"), endOfLife0.some))),  // from previous test
        board0.path \ NoticeKey("2222-02-02") -> NoticePlace(
          Some(notice))))

    "Detach order when at Agent" in:
      // TODO Post kann am Agenten ausgeführt werden, wenn GlobalBoard (ohne BoardState)
      //  dahin übertragen wird, und anschließend der Controller Order.ExpectingNotice löst.
      val qualifier = nextQualifier()
      val notice = Notice(board0.path \ NoticeKey(qualifier), endOfLife0.some)

      val posterEvents = controller.runOrder(
        FreshOrder(OrderId(s"#$qualifier#POSTING"), postingAgentWorkflow.path))
      assert(posterEvents.map(_.value) == Seq(
        OrderAdded(postingAgentWorkflow.id),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderNoticePosted(board0.path \ NoticeKey(qualifier),
          endOfLife = ts"2222-10-11T00:00:00Z".some),
        OrderMoved(Position(2)),
        OrderFinished()))

      val readerEvents = controller.runOrder(
        FreshOrder(OrderId(s"#$qualifier#EXPECTING"), expectingAgentWorkflow.path))
      assert(readerEvents.map(_.value) == Seq(
        OrderAdded(expectingAgentWorkflow.id),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderNoticesRead,
        OrderMoved(Position(2)),
        OrderFinished()))

    "PostNotices command with expecting orders" in:
      val qualifier = "2222-08-08"
      val noticeId = PlanId.Global / board0.path / NoticeKey(qualifier)

      val orderIds = Seq(OrderId(s"#$qualifier#EXPECTING-A"), OrderId(s"#$qualifier#EXPECTING-B"))
      controller.api.addOrders(
        Stream.iterable(orderIds).map(FreshOrder(_, expectingAgentWorkflow.path))
      ).await(99.s).orThrow
      for orderId <- orderIds do eventWatch.await[OrderNoticesExpected](_.key == orderId)
      controller.api.executeCommand:
        ControllerCommand.PostNotice(noticeId)
      .await(99.s).orThrow
      for orderId <- orderIds do eventWatch.await[OrderNoticesRead](_.key == orderId)

      val notice2 = Notice(board0.path \ NoticeKey("2222-08-09"), endOfLife0.some)
      controller.api.executeCommand(
        ControllerCommand.PostNotice(notice2.id, notice2.endOfLife)
      ).await(99.s).orThrow
      assert(controllerState.toNoticePlace(notice2.id) ==
        NoticePlace(Some(notice2)))

    "PostNotices command without expecting order" in:
      val notice = Notice(board0.path \ NoticeKey("2222-08-09"), endOfLife0.some)
      controller.api.executeCommand(
        ControllerCommand.PostNotice(notice.id, notice.endOfLife)
      ).await(99.s).orThrow

      // With explicit endOfLife
      val notice2 = Notice(board0.path \ NoticeKey("2222-08-10"), Some(clock.now() + 1.h))
      controller.api.executeCommand(
        ControllerCommand.PostNotice(notice2.id, notice2.endOfLife)
      ).await(99.s).orThrow

      assert(controllerState.toNoticePlace(notice.id) ==
        NoticePlace(Some(notice)))
      assert(controllerState.toNoticePlace(notice2.id) ==
        NoticePlace(Some(notice2)))

    "DeleteNotice command" in:
      val qualifier = "2222-09-09"
      val notice = Notice(board0.path \ NoticeKey(qualifier), endOfLife0.some)

      val posterEvents = controller.runOrder(
        FreshOrder(OrderId(s"#$qualifier#POSTING"), posting0Workflow.path))
      assert(posterEvents.map(_.value) == Seq(
        OrderAdded(posting0Workflow.id),
        OrderStarted,
        OrderNoticePosted(
          board0.path \ NoticeKey(qualifier),
          endOfLife = ts"2222-10-11T00:00:00Z".some),
        OrderMoved(Position(1)),
        OrderFinished()))

      val eventId = eventWatch.lastAddedEventId

      assert:
        controller.api.executeCommand:
          DeleteNotice(PlanId.Global / board0.path / "UNKNOWN")
        .await(99.s) ==
          Left(UnknownKeyProblem("NoticeKey", "NoticeKey:UNKNOWN"))

      assert:
        controller.api.executeCommand:
          DeleteNotice(BoardPath("UNKNOWN") / notice.plannedNoticeKey)
        .await(99.s) ==
          Left(UnknownKeyProblem("BoardPath", "Board:UNKNOWN"))

      execCmd(DeleteNotice(notice.id))
      assert(eventWatch.await[NoticeDeleted](_.key == board0.path, after = eventId).head.value.event ==
        NoticeDeleted(notice.plannedNoticeKey))

    "Delete notice after endOfLife" in:
      clock := endOfLife0 - 1.s
      sleep(100.ms)
      val eventId = eventWatch.lastAddedEventId
      // NoticeDeleted do not occur before endOfLife
      clock := endOfLife0
      // Spare plannedNoticeKeys.head for DeleteNotice test
      for plannedNoticeKey <- plannedNoticeKeys do
        eventWatch.await[NoticeDeleted](_.event.plannedNoticeKey == plannedNoticeKey, after = eventId)

    "PostNotices and ExpectNotices respect Order.scheduledFor" in:
      val qualifier = "2222-10-10"
      val posterOrderId = OrderId(s"#$qualifier#POSTER")
      val expectingOrderId = OrderId(s"#$qualifier#EXPECTING")
      val startAt = startTimestamp + 10.days

      controller.api
        .addOrders(Stream(
          FreshOrder(posterOrderId, posting0Workflow.path, scheduledFor = Some(startAt)),
          FreshOrder(expectingOrderId, expecting0Workflow.path, scheduledFor = Some(startAt))))
        .await(99.s).orThrow
      sleep(500.ms)
      val idToOrder = controller.api.controllerState.await(99.s).orThrow.idToOrder
      assert(idToOrder(posterOrderId).isState[Fresh] && idToOrder(expectingOrderId).isState[Fresh])

      clock := startAt
      eventWatch.await[OrderFinished](_.key == posterOrderId)
      eventWatch.await[OrderFinished](_.key == expectingOrderId)

    "Order.ExpectingNotice is suspendible" in:
      val qualifier = "2222-11-11"
      val postingOrderId = OrderId(s"#$qualifier#SUSPENDIBLE-POSTING")
      val expectingOrderId = OrderId(s"#$qualifier#SUSPENDIBLE-EXPECTING")
      controller.api.addOrder(FreshOrder(expectingOrderId, expecting0Workflow.path))
        .await(99.s).orThrow
      eventWatch.await[OrderNoticesExpected](_.key == expectingOrderId)

      execCmd(SuspendOrders(Seq(expectingOrderId)))
      eventWatch.await[OrderSuspensionMarked](_.key == expectingOrderId)

      val eventId = eventWatch.lastAddedEventId
      controller.api.addOrder(FreshOrder(postingOrderId, posting0Workflow.path))
        .await(99.s).orThrow
      eventWatch.await[OrderNoticesRead](_.key == expectingOrderId)
      eventWatch.await[OrderMoved](_.key == expectingOrderId, after = eventId)
      eventWatch.await[OrderSuspended](_.key == expectingOrderId, after = eventId)

      execCmd(ResumeOrder(expectingOrderId))
      eventWatch.await[OrderFinished](_.key == expectingOrderId)

    "Order.ExpectingNotice is cancelable" in:
      val board = GlobalBoard.joc(BoardPath("CANCEL-EXPECT"), Some(1.h))
      val workflow = Workflow(WorkflowPath("CANCEL-EXPECT"), Seq(
        ExpectNotices(ExpectNotice(board.path))))

      val eventId = eventWatch.lastAddedEventId
      val qualifier = "2222-12-12"
      val noticeId = PlanId.Global / board.path / NoticeKey(qualifier)
      val orderId = OrderId(s"#$qualifier#CANCEL-EXPECT")

      val Some(versionId) = updateItems(board, workflow): @unchecked

      val expectedBoardState = BoardState(
        board.copy(itemRevision = Some(ItemRevision(0))))

      assert(controllerState.keyTo(BoardState)(board.path) == expectedBoardState)

      controller.api
        .addOrder:
          FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
        .await(99.s).orThrow
      eventWatch.await[OrderNoticesExpected](_.key == orderId, after = eventId)

      execCmd(CancelOrders(Seq(orderId)))
      eventWatch.await[OrderCancelled](_.key == orderId, after = eventId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId, eventId) == Seq(
        OrderAdded(workflow.path ~ versionId, deleteWhenTerminated = true),
        OrderStarted,
        OrderNoticesExpected(Vector(board.path \ NoticeKey(qualifier))),
        OrderStateReset,
        OrderCancelled,
        OrderDeleted))

      assert(controllerState.keyTo(BoardState)(board.path) == expectedBoardState)

      deleteItems(board.path, workflow.path)

    "TransferOrders of Order.ExpectingNotice" in:
      val eventId = eventWatch.lastAddedEventId
      testTransferOrders(boardPath => ExpectNotices(ExpectNotice(boardPath))):
        (board1, board2, workflowId1, workflowId2, orderId, noticeKey) =>
          assert(eventWatch.eventsByKey[OrderEvent](orderId, eventId) == Seq(
            OrderAdded(workflowId1, deleteWhenTerminated = true),
            OrderStarted,
            OrderNoticesExpected(Vector(board1.path \ noticeKey)),
            OrderStateReset,
            OrderTransferred(workflowId2 /: Position(0)),
            OrderNoticesExpected(Vector(board2.path \ noticeKey)),
            OrderNoticesRead,
            OrderMoved(Position(1)),
            OrderFinished(),
            OrderDeleted))

          val endOfLife = controllerState.toNoticePlace(board2.path \ noticeKey).notice.get.endOfLife
          assert(controllerState.toNoticePlace.filterKeys(_.boardPath == board2.path).toMap == Map(
            board2.path \ noticeKey ->
              NoticePlace(Some(Notice(board2.path \ noticeKey, endOfLife)))))
  }

  "Update GlobalBoard" in:
    val boardState = controllerState.keyTo(BoardState)(board0.path)

    val updatedBoard = board0.copy(postOrderToNoticeKey = expr("orderId"))
    controller.api.updateUnsignedSimpleItems(Seq(updatedBoard)).await(99.s).orThrow

    assert(controllerState.keyTo(BoardState)(board0.path) ==
      boardState.copy(
        board = updatedBoard.withRevision(Some(ItemRevision(1)))))

  "Delete GlobalBoard" in:
    val checked = controller
      .updateItemsAsSystemUser(Stream(
        DeleteSimple(board0.path),
        DeleteSimple(board1.path),
        DeleteSimple(board2.path)))
      .await(99.s)
    assert(checked == Left(Problem.Combined(Set(
      ItemIsStillReferencedProblem(board0.path, expecting0Workflow.id, " with 4 Orders"),
      ItemIsStillReferencedProblem(board0.path, expecting01Workflow.id, " with 2 Orders"),
      ItemIsStillReferencedProblem(board0.path, expecting012Workflow.id),
      ItemIsStillReferencedProblem(board0.path, expecting02Workflow.id, " with Order:#2222-01-01#EXPECTING-0-2"),
      ItemIsStillReferencedProblem(board0.path, expectingAgentWorkflow.id, " with 3 Orders, attached to Agent:AGENT"),
      ItemIsStillReferencedProblem(board0.path, posting0Workflow.id, " with 5 Orders"),
      ItemIsStillReferencedProblem(board0.path, postingAgentWorkflow.id, " with Order:#2222-03-03#POSTING, attached to Agent:AGENT"),

      ItemIsStillReferencedProblem(board1.path, expecting01Workflow.id, " with 2 Orders"),
      ItemIsStillReferencedProblem(board1.path, expecting012Workflow.id),
      ItemIsStillReferencedProblem(board1.path, posting12Workflow.id, " with Order:#2222-01-01#POSTING-1-2"),

      ItemIsStillReferencedProblem(board2.path, expecting012Workflow.id),
      ItemIsStillReferencedProblem(board2.path, expecting02Workflow.id, " with Order:#2222-01-01#EXPECTING-0-2"),
      ItemIsStillReferencedProblem(board2.path, posting12Workflow.id, " with Order:#2222-01-01#POSTING-1-2")))))

    controller.api
      .deleteOrdersWhenTerminated(Stream.iterable(
        controllerState.idToOrder.keys))
      .await(99.s).orThrow
    controller.api
      .updateItems(Stream(
        DeleteSimple(board0.path),
        DeleteSimple(board1.path),
        DeleteSimple(board2.path),
        AddVersion(VersionId("DELETION")),
        RemoveVersioned(expecting0Workflow.path),
        RemoveVersioned(expecting01Workflow.path),
        RemoveVersioned(expecting012Workflow.path),
        RemoveVersioned(expecting02Workflow.path),
        RemoveVersioned(expectingAgentWorkflow.path),
        RemoveVersioned(posting0Workflow.path),
        RemoveVersioned(posting12Workflow.path),
        RemoveVersioned(postingAgentWorkflow.path)))
      .await(99.s).orThrow

    assert(!controllerState.keyTo(BoardState).contains(board0.path))

  "JOC-1446 Bug: Wrong OrderMoved event after OrderNoticeRead" in:
    val board = GlobalBoard.joc(BoardPath("JOC-1446"))

    val postingWorkflow = Workflow(WorkflowPath("JOC-1446-POST"), Seq(
      TryInstruction(
        Workflow.of(EmptyJob.execute(agentPath)),
        Workflow.empty),
      TryInstruction(
        Workflow.of(EmptyJob.execute(agentPath)),
        Workflow.empty),
      PostNotices(Seq(board.path))))

    val expectingWorkflow = Workflow(WorkflowPath("JOC-1446-EXPECT"), Seq(
      ExpectNotices(ExpectNotice(board.path))))

    withItems((board, postingWorkflow, expectingWorkflow)): _ =>
      val day = Timestamp.now.toIsoString.take(10) // yyyy-MM-dd
      val postOrderId = OrderId(s"#$day#JOC-1446-POST")
      val expectOrderId = OrderId(s"#$day#JOC-1446-EXPECT")
      controller.api
        .addOrder(FreshOrder(postOrderId, postingWorkflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      controller.api
        .addOrder(FreshOrder(expectOrderId, expectingWorkflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.await[OrderDeleted](_.key == postOrderId)
      eventWatch.await[OrderDeleted](_.key == expectOrderId)


object GlobalBoardTest:
  import scala.language.implicitConversions

  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)

  private val qualifiers = Seq("2222-01-01", "2222-02-02", "2222-03-03")
  private val plannedNoticeKeys = qualifiers.map(GlobalNoticeKey(_))
  private val nextQualifier: () => String = qualifiers.iterator.next

  // One lifeTime per board
  private val lifeTimes = Seq(1.days, 2.days, 3.days)
  private val startTimestamp = ts"2222-10-10T00:00:00Z"
  private val endOfLifes = lifeTimes.map(GlobalBoardTest.startTimestamp + _)
  private val Seq(endOfLife0, endOfLife1, endOfLife2) = endOfLifes

  private val boards = for (lifetime, i) <- lifeTimes.zipWithIndex yield
    GlobalBoard.joc(BoardPath(s"GLOBAL-BOARD-$i"), Some(lifetime))

  private val Seq(board0, board1, board2) = boards

  final class TestJob extends SemaphoreJob(TestJob)
  private object TestJob extends SemaphoreJob.Companion[TestJob]

  private val expecting0Workflow = Workflow(WorkflowPath("EXPECTING-0") ~ "INITIAL", Seq(
    ExpectNotices(board0.path)))

  private val expecting012Workflow = Workflow(WorkflowPath("EXPECTING-0-1-2") ~ "INITIAL", Seq(
    ExpectNotices(board0.path & board1.path & board2.path)))

  private val expecting01Workflow = Workflow(WorkflowPath("EXPECTING-0-1") ~ "INITIAL", Seq(
    ExpectNotices(board0.path & board1.path)))

  private val expecting02Workflow = Workflow(WorkflowPath("EXPECTING-0-2") ~ "INITIAL", Seq(
    ExpectNotices(board0.path & board2.path)))

  private val posting0Workflow = Workflow(WorkflowPath("POSTING-0") ~ "INITIAL", Seq(
    PostNotices(Seq(board0.path))))

  private val posting12Workflow = Workflow(WorkflowPath("POSTING-1-2") ~ "INITIAL", Seq(
    PostNotices(Seq(board1.path, board2.path))))

  private val postingAgentWorkflow = Workflow(WorkflowPath("POSTING-AT-AGENT") ~ "INITIAL", Seq(
    EmptyJob.execute(agentPath),
    PostNotices(Seq(board0.path))))

  private val expectingAgentWorkflow = Workflow(WorkflowPath("EXPECTING-AT-AGENT") ~ "INITIAL", Seq(
    EmptyJob.execute(agentPath),
    ExpectNotices(board0.path)))
