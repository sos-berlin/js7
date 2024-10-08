package js7.data.execution.workflow.instructions

import cats.syntax.traverse.*
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.board.BoardState
import js7.data.order.Order
import js7.data.order.OrderEvent.OrderNoticesExpected
import js7.data.state.StateView
import js7.data.workflow.instructions.ExpectNotices

private[instructions] final class ExpectNoticesExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = ExpectNotices
  val instructionClass = classOf[ExpectNotices]

  def toEvents(expectNotices: ExpectNotices, order: Order[Order.State], state: StateView) =
    detach(order)
      .orElse:
        start(order)
      .getOrElse(order
        .ifState[Order.Ready]
        .map: order =>
          expectNotices.referencedBoardPaths
            .toVector
            .traverse(state.keyTo(BoardState).checked)
            .flatMap: boardStates =>
              for
                scope <- state.toPureOrderScope(order)
                expected <- boardStates.traverse(boardState =>
                  boardState.board
                    .expectingOrderToNoticeId(scope)
                    .map(OrderNoticesExpected.Expected(boardState.path, _)))
              yield
                expectNotices.tryFulfill(order, expected, state)
                  .ifEmpty:
                    OrderNoticesExpected(expected) :: Nil
        .orElse(order
          .ifState[Order.ExpectingNotices]
          .map: order =>
            if order.state.expected.map(_.boardPath).toSet != expectNotices.referencedBoardPaths
            then
              Left(Problem.pure:
                s"ExpectNotices instruction does not match Order.State: $expectNotices <-> ${order.state}")
            else
              Right:
                expectNotices.tryFulfillExpectingOrder(order, state))
        .getOrElse:
          Right(Nil)
        .map(_.map(order.id <-: _)))
