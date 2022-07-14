package js7.data.execution.workflow.instructions

import cats.syntax.traverse.*
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.toIsEmptyAllOps
import js7.data.board.{BoardPath, BoardState}
import js7.data.execution.workflow.instructions.ExpectNoticesExecutor.*
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticesExpected, OrderNoticesRead}
import js7.data.order.{Order, OrderEvent}
import js7.data.state.StateView
import js7.data.workflow.instructions.ExpectNotices

private[instructions] final class ExpectNoticesExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor
{
  type Instr = ExpectNotices
  val instructionClass = classOf[ExpectNotices]

  def toEvents(expectNotices: ExpectNotices, order: Order[Order.State], state: StateView) =
    detach(order)
      .orElse(start(order))
      .getOrElse(order
        .ifState[Order.Ready]
        .map(order =>
          expectNotices.referencedBoardPaths
            .toVector
            .traverse(state.keyTo(BoardState).checked)
            .flatMap { boardStates =>
              for {
                scope <- state.toPureScope(order)
                expected <- boardStates.traverse(boardState =>
                  boardState.board
                    .expectingOrderToNoticeId(scope)
                    .map(OrderNoticesExpected.Expected(boardState.path, _)))
              } yield
                tryFulfill(expectNotices, order, expected, state)
                  .emptyToNone
                  .getOrElse(OrderNoticesExpected(expected) :: Nil)
            })
        .orElse(order
          .ifState[Order.ExpectingNotices]
          .map(order =>
            if (order.state.expected.map(_.boardPath).toSet != expectNotices.referencedBoardPaths)
              Left(Problem.pure(
                s"Instruction does not match Order.State: $expectNotices <-> ${order.state}"))
            else
              Right(tryFulfillExpectingOrder(expectNotices, order, state))))
        .getOrElse(Right(Nil))
        .map(_.map(order.id <-: _)))
}

private object ExpectNoticesExecutor
{
  def tryFulfillExpectingOrder(
    expectNotices: ExpectNotices,
    order: Order[Order.ExpectingNotices],
    state: StateView,
    postedBoards: Set[BoardPath] = Set.empty)
  : List[OrderEvent.OrderActorEvent] =
    tryFulfill(expectNotices, order, order.state.expected, state, postedBoards)

  private def tryFulfill(
    expectNotices: ExpectNotices,
    order: Order[Order.State],
    expected: Vector[OrderNoticesExpected.Expected],
    state: StateView,
    postedBoards: Set[BoardPath] = Set.empty)
  : List[OrderEvent.OrderActorEvent] =
    if (expectNotices.isFulfilled(postedBoards ++ state.availableNotices(expected)))
      OrderNoticesRead :: OrderMoved(order.position.increment) :: Nil
    else
      Nil
}
