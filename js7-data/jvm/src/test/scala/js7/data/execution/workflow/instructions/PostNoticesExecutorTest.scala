package js7.data.execution.workflow.instructions

import cats.syntax.option.*
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.time.{TestWallClock, Timestamp}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.BoardPathExpressionParser.boardPathExpr
import js7.data.board.{BoardPath, BoardState, GlobalBoard, GlobalNoticeKey, Notice, NoticePlace}
import js7.data.execution.workflow.instructions.PostNoticesExecutorTest.*
import js7.data.order.OrderEvent.{OrderAdded, OrderMoved, OrderNoticePosted, OrderNoticesExpected, OrderNoticesRead, OrderStarted}
import js7.data.order.OrderId
import js7.data.plan.PlanSchemaState
import js7.data.state.ControllerTestStateView
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{ExpectNotices, PostNotices}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import scala.concurrent.duration.*

final class PostNoticesExecutorTest extends OurTestSuite:
  private lazy val executorService = new InstructionExecutorService(TestWallClock(clockTimestamp))

  "PostNotices and ExpectNotices" in:
    var state = ControllerTestStateView.of(
      orders = Some(Nil),
      workflows = Some(Seq(postingWorkflow, expecting02or13Workflow, expecting0Workflow)),
      itemStates = PlanSchemaState.Global +: boards.map(BoardState(_)))

    // PostNotice board0, board1
    locally:
      state = state.applyKeyedEvent(postingOrderId <-: OrderAdded(postingWorkflow.id)).orThrow

      val startedEvents = executorService.toEvents(postingOrderId, state).orThrow
      assert(startedEvents == Seq(postingOrderId <-: OrderStarted))
      state = state.applyKeyedEvents(startedEvents).orThrow

      val events = executorService.toEvents(postingOrderId, state).orThrow
      assert(events == Seq(
        postingOrderId <-: OrderNoticePosted(notice0.boardNoticeKey, notice0.endOfLife),
        postingOrderId <-: OrderNoticePosted(notice1.boardNoticeKey, notice1.endOfLife),
        postingOrderId <-: OrderMoved(Position(1))))

      state = state.applyKeyedEvents(events).orThrow
      assert(state.keyTo(BoardState).toMap == Map(
        board0.path -> BoardState(board0, Map(
          notice0.plannedNoticeKey -> NoticePlace(Some(notice0)))),
        board1.path -> BoardState(board1, Map(
          notice1.plannedNoticeKey -> NoticePlace(Some(notice1)))),
        board2.path -> BoardState(board2),
        board3.path -> BoardState(board3)))

    // ExpectNotices
    locally:
      state = state.applyKeyedEvent(expectingOrderId <-: OrderAdded(expecting02or13Workflow.id)).orThrow

      val startedEvents = executorService.toEvents(expectingOrderId, state).orThrow
      assert(startedEvents == Seq(expectingOrderId <-: OrderStarted))
      state = state.applyKeyedEvents(startedEvents).orThrow

      val events = executorService.toEvents(expectingOrderId, state).orThrow
      assert(events == Seq(
        expectingOrderId <-: OrderNoticesExpected(Vector(
          notice0.boardNoticeKey,
          notice2.boardNoticeKey,
          notice1.boardNoticeKey,
          notice3.boardNoticeKey))))

      state = state.applyKeyedEvents(events).orThrow
      assert(state.keyTo(BoardState).toMap == Map(
        board0.path -> BoardState(board0, Map(
          notice0.plannedNoticeKey -> NoticePlace(
            Some(notice0),
            Set(expectingOrderId)))),
        board1.path -> BoardState(board1, Map(
          notice1.plannedNoticeKey -> NoticePlace(
            Some(notice1),
            Set(expectingOrderId)))),
        board2.path -> BoardState(board2, Map(
          notice2.plannedNoticeKey -> NoticePlace(
            None,
            Set(expectingOrderId)))),
        board3.path -> BoardState(board3, Map(
          notice3.plannedNoticeKey -> NoticePlace(
            None,
            Set(expectingOrderId))))))

    // ExpectNotice with a different, never posted PlannedNoticeKey
    locally:
      state = state.applyKeyedEvent(otherExpectingOrderId <-: OrderAdded(expecting0Workflow.id)).orThrow

      val startedEvents = executorService.toEvents(otherExpectingOrderId, state).orThrow
      assert(startedEvents == Seq(otherExpectingOrderId <-: OrderStarted))
      state = state.applyKeyedEvents(startedEvents).orThrow

      val events = executorService.toEvents(otherExpectingOrderId, state).orThrow
      assert(events == Seq(
        otherExpectingOrderId <-: OrderNoticesExpected(Vector(
          board0.path / otherNotice0.noticeKey))))

      state = state.applyKeyedEvents(events).orThrow
      assert(state.keyTo(BoardState).toMap == Map(
        board0.path -> BoardState(board0, Map(
          notice0.plannedNoticeKey -> NoticePlace(
            Some(notice0),
            Set(expectingOrderId)),
          otherNotice0.plannedNoticeKey -> NoticePlace(
            None,
            Set(otherExpectingOrderId)))),
        board1.path -> BoardState(board1, Map(
          notice1.plannedNoticeKey -> NoticePlace(
            Some(notice1),
            Set(expectingOrderId)))),
        board2.path -> BoardState(board2, Map(
          notice2.plannedNoticeKey -> NoticePlace(
            None,
            Set(expectingOrderId)))),
        board3.path -> BoardState(board3, Map(
          notice3.plannedNoticeKey -> NoticePlace(
            None,
            Set(expectingOrderId))))))

    // PostNotice board2
    locally:
      val events = executorService.toEvents(postingOrderId, state).orThrow
      assert(events == Seq(
        postingOrderId <-: OrderNoticePosted(notice2.boardNoticeKey, notice2.endOfLife),
        postingOrderId <-: OrderMoved(Position(2)),
        expectingOrderId <-: OrderNoticesRead,
        expectingOrderId <-: OrderMoved(Position(1)),
      ))

      state = state.applyKeyedEvents(events).orThrow
      assert(state.keyTo(BoardState).toMap == Map(
        board0.path -> BoardState(board0, Map(
          notice0.plannedNoticeKey -> NoticePlace(
            Some(notice0)),
          otherNotice0.plannedNoticeKey -> NoticePlace(
            None,
            Set(otherExpectingOrderId)))),
        board1.path -> BoardState(board1, Map(
          notice1.plannedNoticeKey -> NoticePlace(Some(notice1)))),
        board2.path -> BoardState(board2, Map(
          notice2.plannedNoticeKey -> NoticePlace(Some(notice2)))),
        board3.path -> BoardState(board3)))


object PostNoticesExecutorTest:
  private val clockTimestamp = ts"2111-01-01T00:00:00Z"
  private val lifeTime = 24.h

  // Each board calculates a different lifetime
  private val endOfLife0 = clockTimestamp + 1 * lifeTime
  private val endOfLife1 = clockTimestamp + 2 * lifeTime
  private val endOfLife2 = clockTimestamp + 3 * lifeTime
  private val endOfLife3 = clockTimestamp + 4 * lifeTime

  // Posting OrderId matches #yyyy-mm-dd#...
  private def postingOrderToNoticeId(i: Int) = expr(
    s"""match(orderId, '#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*', '$$1-$i')""")

  // Expecting OrderId matches %yyyy-mm-dd%...
  private def expectingOrderToNoticeId(i: Int) = expr(
    s"""match(orderId, '%([0-9]{4}-[0-9]{2}-[0-9]{2})%.*', '$$1-$i')""")

  private val boards = for i <- 0 to 3 yield
    GlobalBoard(
      BoardPath(s"BOARD-$i"),
      postOrderToNoticeKey = postingOrderToNoticeId(i),
      endOfLife = expr(s"$$js7EpochMilli + ($i + 1) * ${lifeTime.toMillis}"),
      expectOrderToNoticeKey = expectingOrderToNoticeId(i))

  private val Seq(board0, board1, board2, board3) = boards

  // This is for the posting and the expecting order
  private val qualifier = "2222-01-01"
  private val postingOrderId = OrderId(s"#$qualifier#POST")
  private val expectingOrderId = OrderId(s"%$qualifier%EXPECT")
  private val notice0 = Notice(board0.path / GlobalNoticeKey(s"$qualifier-0"), endOfLife0.some)
  private val notice1 = Notice(board1.path / GlobalNoticeKey(s"$qualifier-1"), endOfLife1.some)
  private val notice2 = Notice(board2.path / GlobalNoticeKey(s"$qualifier-2"), endOfLife2.some)
  private val notice3 = Notice(board3.path / GlobalNoticeKey(s"$qualifier-3"), endOfLife3.some)

  // This is for a different expecting, endlessly waiting Order
  private val otherQualifier = "3333-11-11"
  private val otherExpectingOrderId = OrderId(s"%$otherQualifier%OTHER")
  private val otherNotice0 = Notice(board0.path / GlobalNoticeKey(s"$otherQualifier-0"), endOfLife0.some)

  private val postingWorkflow = Workflow(WorkflowPath("POSTING") ~ "1", Seq(
    PostNotices(Seq(board0.path, board1.path)),
    PostNotices(Seq(board2.path))))

  private val expecting02or13Workflow = Workflow(WorkflowPath("EXPECTING-0-OR-1-2") ~ "1", Seq(
    ExpectNotices(boardPathExpr(
      s"'${board0.path.string}' && " +
      s"'${board2.path.string}' || " +
      s"'${board1.path.string}' && " +
      s"'${board3.path.string}'"))))

  private val expecting0Workflow = Workflow(WorkflowPath("EXPECTING-0") ~ "1", Seq(
    ExpectNotices(boardPathExpr(
      s"'${board0.path.string}'"))))
