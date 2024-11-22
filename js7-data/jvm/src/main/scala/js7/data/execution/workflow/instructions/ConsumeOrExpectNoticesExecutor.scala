package js7.data.execution.workflow.instructions

import cats.syntax.traverse.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.ifEmpty
import js7.data.board.{BoardState, GlobalBoard, PlannableBoard}
import js7.data.event.KeyedEvent
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderActorEvent, OrderMoved, OrderNoticesExpected}
import js7.data.state.StateView
import js7.data.workflow.instructions.{ConsumeNotices, ExpectNotices}

trait ConsumeOrExpectNoticesExecutor extends EventInstructionExecutor:

  type Instr <: ConsumeNotices | ExpectNotices

  final def toEvents(instr: Instr, order: Order[Order.State], state: StateView)
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    detach(order)
      .orElse:
        start(order)
      .orElse:
        order.ifState[Order.Ready].map: order =>
          instr.referencedBoardPaths.toVector
            .traverse:
              state.keyTo(BoardState).checked
            .flatMap: boardStates =>
              boardStates.traverse: boardState =>
                boardState.board.match
                  case board: GlobalBoard =>
                    for
                      scope <- state.toOrderScope(order)
                      noticeId <- board.expectingOrderToNoticeId(scope)
                    yield
                      Right(OrderNoticesExpected.Expected(board.path, noticeId))

                  case board: PlannableBoard =>
                    state.orderToPlannableBoardNoticeId(order).map: noticeId =>
                      if boardState.isAnnounced(noticeId) then
                        Right(OrderNoticesExpected.Expected(board.path, noticeId))
                      else
                        Left(OrderMoved(order.position.increment))
              .map(_.sequence)
              .map:
                case Right(expected) =>
                  instr.tryFulfill(order, expected, state)
                    .ifEmpty:
                      OrderNoticesExpected(expected) :: Nil
                    .map(order.id <-: _)
                case Left(moved) =>
                  (order.id <-: moved) :: Nil

      .orElse:
        order.ifState[Order.ExpectingNotices].map: order =>
          if order.state.expected.map(_.boardPath).toSet != instr.referencedBoardPaths then
            Left(Problem.pure(s"${instr.getClass.shortClassName
              } instruction does not match Order.State: $instr <-> ${order.state}"))
          else
            Right:
              instr.tryFulfillExpectingOrder(order, state)
                .map(order.id <-: _)
      .getOrElse:
        Right(Nil)
