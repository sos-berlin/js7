package js7.data.execution.workflow.instructions

import cats.syntax.traverse.*
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.toIsEmptyAllOps
import js7.data.board.{BoardPath, BoardState}
import js7.data.execution.workflow.instructions.ConsumeNoticesExecutor.*
import js7.data.order.OrderEvent.{OrderDetachable, OrderNoticesConsumed, OrderNoticesConsumptionStarted, OrderNoticesExpected}
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
      .orElse(start(order))
      .getOrElse(order
        .ifState[Order.Ready]
        .map(order =>
          instr.referencedBoardPaths
            .toVector
            .traverse(state.keyTo(BoardState).checked)
            .flatMap(boardStates =>
              for
                scope <- state.toPureOrderScope(order)
                expected <- boardStates.traverse(boardState =>
                  boardState.board
                    .expectingOrderToNoticeId(scope)
                    .map(OrderNoticesExpected.Expected(boardState.path, _)))
              yield
                tryFulfill(instr, expected, state)
                  .emptyToNone
                  .getOrElse(OrderNoticesExpected(expected) :: Nil)))
        .orElse(order
          .ifState[Order.ExpectingNotices]
          .map(order =>
            if order.state.expected.map(_.boardPath).toSet != instr.referencedBoardPaths then
              Left(Problem.pure(
                s"Instruction does not match Order.State: $instr <-> ${order.state}"))
            else
              Right(tryFulfillExpectingOrder(instr, order, state))))
        .getOrElse(Right(Nil))
        .map(_.map(order.id <-: _)))

  override def onReturnFromSubworkflow(
    instr: ConsumeNotices,
    order: Order[Order.State],
    state: StateView) =
    Right(List(
      order.id <-: (
        if order.isAttached then
          OrderDetachable
        else
          OrderNoticesConsumed())))

private object ConsumeNoticesExecutor:
  def tryFulfillExpectingOrder(
    instr: ConsumeNotices,
    order: Order[Order.ExpectingNotices],
    state: StateView,
    postedBoards: Set[BoardPath] = Set.empty)
  : List[OrderEvent.OrderActorEvent] =
    tryFulfill(instr, order.state.expected, state, postedBoards)

  private def tryFulfill(
    instr: ConsumeNotices,
    expected: Vector[OrderNoticesExpected.Expected],
    state: StateView,
    postedBoards: Set[BoardPath] = Set.empty)
  : List[OrderEvent.OrderActorEvent] =
    if instr.isFulfilled(postedBoards ++ state.availableNotices(expected)) then
      OrderNoticesConsumptionStarted(expected) :: Nil
    else
      Nil
