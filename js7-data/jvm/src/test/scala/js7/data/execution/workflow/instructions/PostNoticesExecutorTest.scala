package js7.data.execution.workflow.instructions

import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.{TestWallClock, Timestamp}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.BoardPathExpressionParser.boardPathExpr
import js7.data.board.{BoardPath, BoardState, GlobalBoard, Notice, NoticeId, NoticePlace}
import js7.data.execution.workflow.instructions.PostNoticesExecutorTest.*
import js7.data.order.OrderEvent.{OrderAdded, OrderMoved, OrderNoticePosted, OrderNoticesExpected, OrderNoticesRead, OrderStarted}
import js7.data.order.OrderId
import js7.data.state.TestStateView
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{ExpectNotices, PostNotices}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import scala.concurrent.duration.*

final class PostNoticesExecutorTest extends OurTestSuite:
  private lazy val executorService = new InstructionExecutorService(TestWallClock(clockTimestamp))

  "PostNotices and ExpectNotices" in:
    var state = TestStateView.of(
      isAgent = false,
      orders = Some(Nil),
      workflows = Some(Seq(postingWorkflow, expecting02or13Workflow, expecting0Workflow)),
      itemStates = boards.map(BoardState(_)))

    // PostNotice board0, board1
    locally:
      state = state.applyEvent(postingOrderId <-: OrderAdded(postingWorkflow.id)).orThrow

      val startedEvents = executorService.toEvents(postingOrderId, state).orThrow
      assert(startedEvents == Seq(postingOrderId <-: OrderStarted))
      state = state.applyEvents(startedEvents).orThrow

      val events = executorService.toEvents(postingOrderId, state).orThrow
      assert(events == Seq(
        postingOrderId <-: OrderNoticePosted(notice0),
        postingOrderId <-: OrderNoticePosted(notice1),
        postingOrderId <-: OrderMoved(Position(1))))

      state = state.applyEvents(events).orThrow
      assert(state.keyTo(BoardState).toMap == Map(
        board0.path -> BoardState(board0, Map(
          notice0.id -> NoticePlace(notice0.id, Some(notice0)))),
        board1.path -> BoardState(board1, Map(
          notice1.id -> NoticePlace(notice1.id, Some(notice1)))),
        board2.path -> BoardState(board2),
        board3.path -> BoardState(board3)))

    // ExpectNotices
    locally:
      state = state.applyEvent(expectingOrderId <-: OrderAdded(expecting02or13Workflow.id)).orThrow

      val startedEvents = executorService.toEvents(expectingOrderId, state).orThrow
      assert(startedEvents == Seq(expectingOrderId <-: OrderStarted))
      state = state.applyEvents(startedEvents).orThrow

      val events = executorService.toEvents(expectingOrderId, state).orThrow
      assert(events == Seq(
        expectingOrderId <-: OrderNoticesExpected(Vector(
          OrderNoticesExpected.Expected(board0.path, notice0.id),
          OrderNoticesExpected.Expected(board2.path, notice2.id),
          OrderNoticesExpected.Expected(board1.path, notice1.id),
          OrderNoticesExpected.Expected(board3.path, notice3.id)))))

      state = state.applyEvents(events).orThrow
      assert(state.keyTo(BoardState).toMap == Map(
        board0.path -> BoardState(board0, Map(
          notice0.id -> NoticePlace(
            notice0.id,
            Some(notice0),
            Set(expectingOrderId)))),
        board1.path -> BoardState(board1, Map(
          notice1.id -> NoticePlace(
            notice1.id,
            Some(notice1),
            Set(expectingOrderId)))),
        board2.path -> BoardState(board2, Map(
          notice2.id -> NoticePlace(
            notice2.id,
            None,
            Set(expectingOrderId)))),
        board3.path -> BoardState(board3, Map(
          notice3.id -> NoticePlace(
            notice3.id,
            None,
            Set(expectingOrderId))))))

    // ExpectNotice with a different, never posted NoticeId
    locally:
      state = state.applyEvent(otherExpectingOrderId <-: OrderAdded(expecting0Workflow.id)).orThrow

      val startedEvents = executorService.toEvents(otherExpectingOrderId, state).orThrow
      assert(startedEvents == Seq(otherExpectingOrderId <-: OrderStarted))
      state = state.applyEvents(startedEvents).orThrow

      val events = executorService.toEvents(otherExpectingOrderId, state).orThrow
      assert(events == Seq(
        otherExpectingOrderId <-: OrderNoticesExpected(Vector(
          OrderNoticesExpected.Expected(board0.path, otherNotice0.id)))))

      state = state.applyEvents(events).orThrow
      assert(state.keyTo(BoardState).toMap == Map(
        board0.path -> BoardState(board0, Map(
          notice0.id -> NoticePlace(
            notice0.id,
            Some(notice0),
            Set(expectingOrderId)),
          otherNotice0.id -> NoticePlace(
            otherNotice0.id,
            None,
            Set(otherExpectingOrderId)))),
        board1.path -> BoardState(board1, Map(
          notice1.id -> NoticePlace(
            notice1.id,
            Some(notice1),
            Set(expectingOrderId)))),
        board2.path -> BoardState(board2, Map(
          notice2.id -> NoticePlace(
            notice2.id,
            None,
            Set(expectingOrderId)))),
        board3.path -> BoardState(board3, Map(
          notice3.id -> NoticePlace(
            notice3.id,
            None,
            Set(expectingOrderId))))))

    // PostNotice board2
    locally:
      val events = executorService.toEvents(postingOrderId, state).orThrow
      assert(events == Seq(
        postingOrderId <-: OrderNoticePosted(notice2),
        postingOrderId <-: OrderMoved(Position(2)),
        expectingOrderId <-: OrderNoticesRead,
        expectingOrderId <-: OrderMoved(Position(1)),
      ))

      state = state.applyEvents(events).orThrow
      assert(state.keyTo(BoardState).toMap == Map(
        board0.path -> BoardState(board0, Map(
          notice0.id -> NoticePlace(
            notice0.id,
            Some(notice0)),
          otherNotice0.id -> NoticePlace(
            otherNotice0.id,
            None,
            Set(otherExpectingOrderId)))),
        board1.path -> BoardState(board1, Map(
          notice1.id -> NoticePlace(
            notice1.id, Some(notice1)))),
        board2.path -> BoardState(board2, Map(
          notice2.id -> NoticePlace(
            notice2.id, Some(notice2)))),
        board3.path -> BoardState(board3)))


object PostNoticesExecutorTest:
  private val clockTimestamp = Timestamp("2111-01-01T00:00:00Z")
  private val lifeTime = 24.h

  // Each board calculates a different lifetime
  private val endOfLife0 = clockTimestamp + 1 * lifeTime
  private val endOfLife1 = clockTimestamp + 2 * lifeTime
  private val endOfLife2 = clockTimestamp + 3 * lifeTime
  private val endOfLife3 = clockTimestamp + 4 * lifeTime

  // Posting OrderId matches #yyyy-mm-dd#...
  private def postingOrderToNoticeId(i: Int) = expr(
    s"""replaceAll($$js7OrderId, '^#([0-9]{4}-[0-9]{2}-[0-9]{2})#.*$$', '$$1-$i')""")

  // Expecting OrderId matches %yyyy-mm-dd%...
  private def expectingOrderToNoticeId(i: Int) = expr(
    s"""replaceAll($$js7OrderId, '^%([0-9]{4}-[0-9]{2}-[0-9]{2})%.*$$', '$$1-$i')""")

  private val boards = for i <- 0 to 3 yield
    GlobalBoard(
      BoardPath(s"BOARD-$i"),
      postOrderToNoticeId = postingOrderToNoticeId(i),
      endOfLife = expr(s"$$js7EpochMilli + ($i + 1) * ${lifeTime.toMillis}"),
      expectOrderToNoticeId = expectingOrderToNoticeId(i))

  private val Seq(board0, board1, board2, board3) = boards

  // This is for the posting and the expecting order
  private val qualifier = "2222-01-01"
  private val postingOrderId = OrderId(s"#$qualifier#POST")
  private val expectingOrderId = OrderId(s"%$qualifier%EXPECT")
  private val notice0 = Notice(NoticeId(s"$qualifier-0"), board0.path, endOfLife0)
  private val notice1 = Notice(NoticeId(s"$qualifier-1"), board1.path, endOfLife1)
  private val notice2 = Notice(NoticeId(s"$qualifier-2"), board2.path, endOfLife2)
  private val notice3 = Notice(NoticeId(s"$qualifier-3"), board3.path, endOfLife3)

  // This is for a different expecting, endlessly waiting Order
  private val otherQualifier = "3333-11-11"
  private val otherExpectingOrderId = OrderId(s"%$otherQualifier%OTHER")
  private val otherNotice0 = Notice(NoticeId(s"$otherQualifier-0"), board0.path, endOfLife0)

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
