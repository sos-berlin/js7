package js7.data.execution.workflow.instructions

import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.board.{BoardState, Notice, NoticeId}
import js7.data.execution.workflow.context.StateView
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderActorEvent, OrderDetachable, OrderMoved, OrderNoticeAwaiting, OrderNoticeRead, OrderStarted}
import js7.data.workflow.instructions.ReadNotice
import js7.data.workflow.position.Position

object ReadNoticeExecutor extends EventInstructionExecutor
{
  type Instr = ReadNotice

  def toEvents(instruction: ReadNotice, order: Order[Order.State], state: StateView) = {
    import instruction.boardPath

    val events =
      if (order.isAttached)
        Right(OrderDetachable :: Nil)
      else if (order.isState[Order.Fresh] && order.isDetached)
        Right(OrderStarted :: Nil)
      else
        state.pathToBoardState
          .checked(boardPath)
          .flatMap(boardState =>
            order.state match {
              case _: Order.Fresh =>
                Right(OrderStarted :: Nil)

              case _: Order.Ready =>
                for {
                  scope <- state.toScope(order)
                  noticeId <- boardState.board.readingOrderToNoticeId(scope)
                } yield
                  tryRead(boardState, noticeId, order.position)
                    .getOrElse(OrderNoticeAwaiting(noticeId) :: Nil)

              case Order.WaitingForNotice(noticeId) =>
                Right(
                  tryRead(boardState, noticeId, order.position)
                    .toList
                    .flatten)

              case _ => Right(Nil)
            })

    events.map(_.map(order.id <-: _))
  }

  private def tryRead(boardState: BoardState, noticeId: NoticeId, position: Position)
  : Option[List[OrderActorEvent]] =
    boardState.idToNotice.get(noticeId).map {
      case _: Notice => OrderNoticeRead :: OrderMoved(position.increment) :: Nil
      case _ => Nil
    }
}
