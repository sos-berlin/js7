package js7.data.execution.workflow.instructions

import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.base.utils.typeclasses.IsEmpty.syntax.toIsEmptyAllOps
import js7.data.board.{BoardState, Notice, NoticeId}
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderActorEvent, OrderMoved, OrderNoticeExpected, OrderNoticeRead}
import js7.data.state.StateView
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
              case _: Order.Ready =>
                for {
                  scope <- state.toScope(order)
                  noticeId <- boardState.board.expectingOrderToNoticeId(scope)
                } yield
                  tryRead(boardState, noticeId, order.position)
                    .emptyToNone
                    .getOrElse(OrderNoticeExpected(noticeId) :: Nil)

              case Order.ExpectingNotice(noticeId) =>
                Right(
                  tryRead(boardState, noticeId, order.position))

              case _ => Right(Nil)
            })
          .map(_.map(order.id <-: _)))
  }

  private def tryRead(boardState: BoardState, noticeId: NoticeId, position: Position)
  : List[OrderActorEvent] =
    boardState.idToNotice.get(noticeId).toList.flatMap {
      case _: Notice => OrderNoticeRead :: OrderMoved(position.increment) :: Nil
      case _ => Nil
    }
}
