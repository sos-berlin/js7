package js7.data.execution.workflow.instructions

import cats.syntax.semigroup._
import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.base.time.WallClock
import js7.base.utils.ScalaUtils.syntax._
import js7.data.execution.workflow.context.StateView
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderDetachable, OrderMoved, OrderNoticePosted, OrderNoticeRead, OrderStarted}
import js7.data.value.expression.scopes.NowScope
import js7.data.workflow.instructions.PostNotice
import scala.collection.View

final class PostNoticeExecutor(clock: WallClock) extends EventInstructionExecutor
{
  type Instr = PostNotice

  def toEvents(instruction: PostNotice, order: Order[Order.State], state: StateView) =
    if (order.isAttached)
      Right((order.id <-: OrderDetachable) :: Nil)
    else if (order.isState[Order.Fresh] && order.isDetached)
      Right((order.id <-: OrderStarted) :: Nil)
    else if (order.isState[Order.Ready]) {
      import instruction.boardPath
      for {
        board <- state.pathToBoard.checked(boardPath)
        orderScope <- state.toScope(order)
        notice <- board.postingOrderToNotice(orderScope |+| NowScope(clock.now()))
        boardState <- state.pathToBoardState.checked(boardPath)
        waitingOrders <- boardState
          .waitingOrders(notice.id)
          .traverse(state.idToOrder.checked): Checked[Seq[Order[Order.State]]]
      } yield
        (order.id <-: OrderNoticePosted(notice)) ::
          (order.id <-: OrderMoved(order.position.increment)) ::
          waitingOrders.view
            .flatMap(o => View(
              o.id <-: OrderNoticeRead,
              o.id <-: OrderMoved(o.position.increment)))
            .toList
    } else
      Right(Nil)
}
