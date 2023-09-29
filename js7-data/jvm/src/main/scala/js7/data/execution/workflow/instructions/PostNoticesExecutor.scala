package js7.data.execution.workflow.instructions

import cats.syntax.traverse.*
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.{BoardState, Notice}
import js7.data.event.KeyedEvent
import js7.data.execution.workflow.instructions.PostNoticesExecutor.*
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticePosted}
import js7.data.order.{Order, OrderEvent}
import js7.data.state.StateView
import js7.data.workflow.instructions.{ExpectOrConsumeNoticesInstruction, PostNotices}

private[instructions] final class PostNoticesExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor:
  type Instr = PostNotices
  val instructionClass = classOf[PostNotices]

  def toEvents(postNotices: PostNotices, order: Order[Order.State], state: StateView) =
    detach(order)
      .orElse(start(order))
      .getOrElse(
        if order.isState[Order.Ready] then
          for
            boardStates <- postNotices.boardPaths.traverse(state.keyTo(BoardState).checked)
            orderScope <- state.toImpureOrderExecutingScope(order, clock.now())
            fatNotices <- boardStates.traverse(bs => bs.board
              .postingOrderToNotice(orderScope)
              .map(FatNotice(_, bs)))
            postingOrderEvents = toPostingOrderEvents(fatNotices.map(_.notice), order)
            expectingOrderEvents <- toExpectingOrderEvents(fatNotices, state)
          yield
            postingOrderEvents ++: expectingOrderEvents.toList
        else
          Right(Nil))

object PostNoticesExecutor:
  private final case class FatNotice(notice: Notice, boardState: BoardState)

  private def toPostingOrderEvents(notices: Vector[Notice], order: Order[Order.State])
  : Vector[KeyedEvent[OrderEvent.OrderActorEvent]] =
    notices
      .map(n => order.id <-: OrderNoticePosted(n))
      .appended(order.id <-: OrderMoved(order.position.increment))

  // For PostNotice command
  def postedNoticeToExpectingOrderEvents(boardState: BoardState, notice: Notice, state: StateView)
  : Checked[Seq[KeyedEvent[OrderEvent.OrderActorEvent]]] =
    toExpectingOrderEvents(Vector(FatNotice(notice, boardState)), state)

  private def toExpectingOrderEvents(
    postedNotices: Vector[FatNotice],
    state: StateView)
  : Checked[Vector[KeyedEvent[OrderEvent.OrderActorEvent]]] =
    for
      expectingOrders <- postedNotices
        .flatMap(post => post.boardState.expectingOrders(post.notice.id))
        .distinct
        .traverse(state.idToOrder.checked)
      events <- expectingOrders
        .traverse(expectingOrder => state
          .instruction_[ExpectOrConsumeNoticesInstruction](expectingOrder.workflowPosition)
          .map(expectingOrder -> _))
        .flatMap(_
          .traverse { case (expectingOrder, expectNoticesInstr) =>
            state.idToOrder.checked(expectingOrder.id)
              .flatMap(_.checkedState[Order.ExpectingNotices])
              .map { expectingOrder =>
                val postedBoards = postedNotices.map(_.boardState.path).toSet
                ExpectOrConsumeNoticesExecutor
                  .tryFulfillExpectingOrder(expectNoticesInstr, expectingOrder, state, postedBoards)
                  .map(expectingOrder.id <-: _)
              }
          }
          .map(_.flatten))
    yield events
