package js7.data.execution.workflow.instructions

import cats.syntax.semigroup._
import cats.syntax.traverse._
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import js7.data.execution.workflow.context.StateView
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticePosted, OrderNoticeRead}
import js7.data.value.expression.scopes.NowScope
import js7.data.workflow.instructions.PostNotice
import scala.collection.View

private[instructions] final class PostNoticeExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = PostNotice

  def toEvents(instruction: PostNotice, order: Order[Order.State], state: StateView) =
    detach(order)
      .orElse(start(order))
      .getOrElse(
        if (order.isState[Order.Ready]) {
          import instruction.boardPath
          for {
            board <- state.pathToBoard.checked(boardPath)
            orderScope <- state.toScope(order)
            notice <- board.postingOrderToNotice(orderScope |+| NowScope(clock.now()))
            boardState <- state.pathToBoardState.checked(boardPath)
            expectingOrders <- boardState
              .expectingOrders(notice.id)
              .traverse(state.idToOrder.checked): Checked[Seq[Order[Order.State]]]
          } yield
            (order.id <-: OrderNoticePosted(notice)) ::
              (order.id <-: OrderMoved(order.position.increment)) ::
              expectingOrders.view
                .flatMap(o => View(
                  o.id <-: OrderNoticeRead,
                  o.id <-: OrderMoved(o.position.increment)))
                .toList
        } else
          Right(Nil))
}
