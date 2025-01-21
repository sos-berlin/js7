package js7.data.execution.workflow.instructions

import cats.syntax.traverse.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.ifEmpty
import js7.data.board.{BoardState, GlobalBoard, PlannableBoard}
import js7.data.event.KeyedEvent
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderActorEvent, OrderNoticesExpected}
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
                        .left.map:
                          _.withPrefix(s"${instr.getClass.shortClassName}:")
                    yield
                      noticeId

                  case board: PlannableBoard =>
                    state.emptyPlannedNoticeKey(order).map: plannedNoticeKey =>
                      assertThat(plannedNoticeKey.planId == order.planId)
                      board.path / plannedNoticeKey
              .map: noticeIds =>
                instr.tryFulfill(order, noticeIds, state.isNoticeAvailable)
                  .ifEmpty:
                    OrderNoticesExpected(noticeIds) :: Nil
                  .map(order.id <-: _)
      .orElse:
        order.ifState[Order.ExpectingNotices].map: order =>
          if order.state.noticeIds.map(_.boardPath).toSet != instr.referencedBoardPaths then
            Left(Problem.pure(s"${instr.getClass.shortClassName
              } instruction does not match Order.State: $instr <-> ${order.state}"))
          else
            Right:
              instr.tryFulfillExpectingOrder(order, state.isNoticeAvailable)
                .map(order.id <-: _)
      .getOrElse:
        Right(Nil)
