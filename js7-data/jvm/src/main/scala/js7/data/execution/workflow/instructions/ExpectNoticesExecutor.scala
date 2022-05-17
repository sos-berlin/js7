package js7.data.execution.workflow.instructions

import cats.syntax.traverse._
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.typeclasses.IsEmpty.syntax.toIsEmptyAllOps
import js7.data.board.{BoardState, NoticeId}
import js7.data.execution.workflow.instructions.ExpectNoticesExecutor._
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderActorEvent, OrderMoved, OrderNoticesExpected, OrderNoticesRead}
import js7.data.state.StateView
import js7.data.workflow.instructions.ExpectNotices
import js7.data.workflow.position.Position

private[instructions] final class ExpectNoticesExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = ExpectNotices
  val instructionClass = classOf[ExpectNotices]

  def toEvents(expectNotices: ExpectNotices, order: Order[Order.State], state: StateView) =
    detach(order)
      .orElse(start(order))
      .getOrElse(
        expectNotices.boardPaths
          .traverse(state.pathToBoardState.checked)
          .flatMap(boardStates =>
            order.state match {
              case _: Order.Ready =>
                for {
                  scope <- state.toPureScope(order)
                  expectations <- boardStates.traverse(boardState =>
                    boardState.board
                      .expectingOrderToNoticeId(scope)
                      .map(StateAndNoticeId(boardState, _)))
                } yield
                  tryRead(expectations, order.position)
                    .emptyToNone
                    .getOrElse(
                      OrderNoticesExpected(expectations
                        .map(x => OrderNoticesExpected.Expected(x.boardState.path, x.noticeId))
                      ) :: Nil)

              case Order.ExpectingNotices(expectings) =>
                if (expectings.map(_.boardPath).toSet != expectNotices.boardPaths.toSet)
                  Left(Problem.pure(
                    s"Instruction does not match Order State: $expectNotices <> ${order.state}"))
                else
                  expectings.traverse(exp => state
                    .pathToBoardState.checked(exp.boardPath))
                    .map(boardStates =>
                      tryRead(
                        boardStates.zip(expectings.map(_.noticeId))
                          .map(StateAndNoticeId.fromPair),
                        order.position))

              case _ => Right(Nil)
            })
          .map(_.map(order.id <-: _)))

  private def tryRead(statesAndNoticeIds: Seq[StateAndNoticeId], position: Position)
  : List[OrderActorEvent] =
    if (allNoticesAvailable(statesAndNoticeIds))
      OrderNoticesRead :: OrderMoved(position.increment) :: Nil
    else
      Nil
}

object ExpectNoticesExecutor
{
  private def allNoticesAvailable(statesAndNoticeIds: Seq[StateAndNoticeId])
  : Boolean =
    statesAndNoticeIds
      .forall(x => x
        .boardState.idToNotice.get(x.noticeId)
        .exists(_.notice.isDefined))

  private final case class StateAndNoticeId(boardState: BoardState, noticeId: NoticeId)
  private object StateAndNoticeId {
    def fromPair(pair: (BoardState, NoticeId)): StateAndNoticeId =
      StateAndNoticeId(pair._1, pair._2)
  }
}
