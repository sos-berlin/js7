package js7.data.board

import cats.syntax.traverse.*
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.time.WallClock
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.NoticeEvent.{NoticeDeleted, NoticePosted}
import js7.data.board.NoticeEventSource.*
import js7.data.controller.ControllerCommand
import js7.data.event.KeyedEvent
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticeEvent, OrderNoticePosted, OrderNoticesConsumptionStarted, OrderNoticesRead}
import js7.data.order.{Order, OrderEvent, OrderId}
import js7.data.state.StateView
import js7.data.value.expression.scopes.NowScope
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction

final class NoticeEventSource(clock: WallClock):

  def postNotices(boardPaths: Vector[BoardPath], order: Order[Order.Ready], state: StateView)
  : Checked[List[KeyedEvent[OrderNoticeEvent | OrderMoved]]] =
    for
      boardStates <- boardPaths.traverse(state.keyTo(BoardState).checked)
      fatNotices <- boardStates.traverse: boardState =>
        boardState.board.match
          case board: GlobalBoard =>
            // A big, unpure Scope ???
            state.toImpureOrderExecutingScope(order, clock.now()).flatMap: scope =>
              board.postingOrderToNotice(scope)

          case board: PlannableBoard =>
            for
              scope <- state.toFreshOrderScope(order)
              noticeId <- state.orderToPlannableBoardNoticeId(order.id, scope)
            yield
              Notice(noticeId, board.path, endOfLife = None)
        .map:
          FatNotice(_, boardState)
      postingOrderEvents = toPostingOrderEvents(fatNotices.map(_.notice), order)
      expectingOrderEvents <- toExpectingOrderEvents(fatNotices, state)
    yield
      postingOrderEvents ++: expectingOrderEvents.toList

  def executePostNoticeCommand(postNotice: ControllerCommand.PostNotice, state: StateView)
  : Checked[Seq[KeyedEvent[OrderNoticeEvent | OrderMoved | NoticeEvent]]] =
    val ControllerCommand.PostNotice(boardPath, noticeId, maybeEndOfLife) = postNotice
    val scope = NowScope(clock.now()) // TODO Should be pure ?
    for
      boardState <- state.keyTo(BoardState).checked(boardPath)
      notice <- boardState.board.toNotice(noticeId, maybeEndOfLife)(scope)
      _ <- boardState.addNotice(notice) // Check
      expectingOrderEvents <-
        if notice.endOfLife.exists(_ <= clock.now()) then
          logger.debug(s"Delete $notice immediately because endOfLife has been reached")
          Right((notice.boardPath <-: NoticeDeleted(notice.id)) :: Nil)
        else
          postedNoticeToExpectingOrderEvents(boardState, notice, state)
    yield
      NoticePosted.toKeyedEvent(notice) +: expectingOrderEvents


object NoticeEventSource:

  private val logger = Logger[this.type]

  private final case class FatNotice(notice: Notice, boardState: BoardState)

  private def toPostingOrderEvents(notices: Vector[Notice], order: Order[Order.State])
  : Vector[KeyedEvent[OrderNoticePosted | OrderMoved]] =
    notices.map: n =>
      order.id <-: OrderNoticePosted(n)
    .appended:
      order.id <-: OrderMoved(order.position.increment)

  // For PostNotice command
  private def postedNoticeToExpectingOrderEvents(
    boardState: BoardState,
    notice: Notice,
    state: StateView)
  : Checked[Seq[KeyedEvent[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved]]] =
    toExpectingOrderEvents(Vector(FatNotice(notice, boardState)), state)

  private def toExpectingOrderEvents(
    postedNotices: Vector[FatNotice],
    state: StateView)
  : Checked[Vector[KeyedEvent[OrderNoticesConsumptionStarted | OrderNoticesRead | OrderMoved]]] =
    for
      expectingOrders <- postedNotices
        .flatMap(post => post.boardState.expectingOrders(post.notice.id))
        .distinct
        .traverse(state.idToOrder.checked)
      events <- expectingOrders
        .traverse(expectingOrder => state
          .instruction_[ExpectOrConsumeNoticesInstruction](expectingOrder.workflowPosition)
          .map(expectingOrder -> _))
        .flatMap:
          _.traverse: (expectingOrder, expectNoticesInstr) =>
            state.idToOrder.checked(expectingOrder.id)
              .flatMap(_.checkedState[Order.ExpectingNotices])
              .map: expectingOrder =>
                val postedBoards = postedNotices.map(_.boardState.path).toSet
                expectNoticesInstr
                  .tryFulfillExpectingOrder(expectingOrder, state, postedBoards)
                  .map(expectingOrder.id <-: _)
          .map(_.flatten)
    yield
      events
