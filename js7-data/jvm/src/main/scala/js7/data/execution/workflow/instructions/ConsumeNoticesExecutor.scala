package js7.data.execution.workflow.instructions

import cats.syntax.traverse.*
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.*
import js7.data.board.{BoardPath, BoardState}
import js7.data.order.OrderEvent.{OrderDetachable, OrderNoticesConsumed, OrderNoticesExpected}
import js7.data.order.{Order, OrderEvent}
import js7.data.state.StateView
import js7.data.workflow.instructions.ConsumeNotices

private[instructions] final class ConsumeNoticesExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor:
  type Instr = ConsumeNotices
  val instructionClass = classOf[ConsumeNotices]

  // TODO Similar to ExpectNoticesExecutor
  def toEvents(instr: ConsumeNotices, order: Order[Order.State], state: StateView) =
    detach(order)
      .orElse:
        start(order)
      .getOrElse(order
        .ifState[Order.Ready]
        .map: order =>
          instr.referencedBoardPaths
            .toVector
            .traverse(state.keyTo(BoardState).checked)
            .flatMap: boardStates =>
              for
                scope <- state.toPureOrderScope(order)
                expected <- boardStates.traverse: boardState =>
                  boardState.board
                    .expectingOrderToNoticeId(scope)
                    .map(OrderNoticesExpected.Expected(boardState.path, _))
              yield
                instr.tryFulfill(order, expected, state)
                  .ifEmpty:
                    OrderNoticesExpected(expected) :: Nil
        .orElse(order
          .ifState[Order.ExpectingNotices]
          .map(order =>
            if order.state.expected.map(_.boardPath).toSet != instr.referencedBoardPaths then
              Left(Problem.pure:
                s"ConsumeNotices instruction does not match Order.State: $instr <-> ${order.state}")
            else
              Right(instr.tryFulfillExpectingOrder(order, state))))
        .getOrElse:
          Right(Nil)
        .map(_.map(order.id <-: _)))

  override def onReturnFromSubworkflow(
    instr: ConsumeNotices,
    order: Order[Order.State],
    state: StateView) =
    Right(List:
      order.id <-: (
        if order.isAttached then
          OrderDetachable
        else
          OrderNoticesConsumed()))
