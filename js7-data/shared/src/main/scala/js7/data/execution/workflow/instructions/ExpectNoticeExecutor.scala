package js7.data.execution.workflow.instructions

import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.board.{BoardState, Notice, NoticeId}
import js7.data.execution.workflow.context.StateView
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderActorEvent, OrderMoved, OrderNoticeExpected, OrderNoticeRead, OrderStarted}
import js7.data.workflow.instructions.ExpectNotice
import js7.data.workflow.position.Position

private[instructions] final class ExpectNoticeExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = ExpectNotice

  def toEvents(instruction: ExpectNotice, order: Order[Order.State], state: StateView) = {
    import instruction.boardPath

    detach(order)
      .orElse(start(order))
      .getOrElse(
        state.pathToBoardState
          .checked(boardPath)
          .flatMap(boardState =>
            order.state match {
              case _: Order.Fresh =>
                Right(OrderStarted :: Nil)

              case _: Order.Ready =>
                for {
                  scope <- state.toScope(order)
                  noticeId <- boardState.board.expectingOrderToNoticeId(scope)
                } yield
                  tryRead(boardState, noticeId, order.position)
                    .getOrElse(OrderNoticeExpected(noticeId) :: Nil)

              case Order.ExpectingNotice(noticeId) =>
                Right(
                  tryRead(boardState, noticeId, order.position)
                    .toList
                    .flatten)

              case _ => Right(Nil)
            })
          .map(_.map(order.id <-: _)))
  }

  private def tryRead(boardState: BoardState, noticeId: NoticeId, position: Position)
  : Option[List[OrderActorEvent]] =
    boardState.idToNotice.get(noticeId).map {
      case _: Notice => OrderNoticeRead :: OrderMoved(position.increment) :: Nil
      case _ => Nil
    }
}
