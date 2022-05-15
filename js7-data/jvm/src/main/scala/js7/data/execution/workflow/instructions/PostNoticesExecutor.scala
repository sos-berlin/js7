package js7.data.execution.workflow.instructions

import cats.syntax.traverse._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticePosted, OrderNoticeRead}
import js7.data.state.StateView
import js7.data.workflow.instructions.PostNotices
import scala.collection.View

private[instructions] final class PostNoticesExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = PostNotices
  val instructionClass = classOf[PostNotices]

  def toEvents(postNotices: PostNotices, order: Order[Order.State], state: StateView) =
    detach(order)
      .orElse(start(order))
      .getOrElse(
        if (order.isState[Order.Ready]) {
          val now = clock.now()
          for {
            boardStates <- postNotices.boardPaths.traverse(state.pathToBoardState.checked)
            orderScope <- state.toImpureOrderExecutingScope(order, now)
            notices <- boardStates.traverse(_.board.postingOrderToNotice(orderScope))
            expectingOrders <- boardStates
              .flatMap(boardState =>
                notices.flatMap(notice => boardState.expectingOrders(notice.id)))
              .distinct
              .traverse(state.idToOrder.checked)
          } yield
            notices
              .map(order.id <-: OrderNoticePosted(_))
              .appended(order.id <-: OrderMoved(order.position.increment))
              .concat(expectingOrders
                .flatMap(o => View(
                  o.id <-: OrderNoticeRead,
                  o.id <-: OrderMoved(o.position.increment))))
              .toList
        } else
          Right(Nil))
}
