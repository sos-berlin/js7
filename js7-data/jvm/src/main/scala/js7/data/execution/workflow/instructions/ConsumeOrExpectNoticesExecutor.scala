package js7.data.execution.workflow.instructions

import cats.syntax.traverse.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.ifEmpty
import js7.data.board.{GlobalBoard, PlannableBoard}
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
              state.pathToBoardItem
            .flatMap: boards =>
              boards.traverse:
                case board: GlobalBoard =>
                  for
                    scope <- state.toOrderScope(order)
                    noticeId <- board.expectingOrderToNoticeId(scope)
                  yield
                    OrderNoticesExpected.Expected(board.path, noticeId)
                case board: PlannableBoard =>
                  for
                    scope <- state.toFreshOrderScope(order)
                    noticeId <- state.orderToPlannableBoardNoticeId(order.id, scope)
                  yield
                    OrderNoticesExpected.Expected(board.path, noticeId)
              .map: expected =>
                instr.tryFulfill(order, expected, state)
                  .ifEmpty:
                    OrderNoticesExpected(expected) :: Nil
                  .map(order.id <-: _)
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
