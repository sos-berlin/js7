package js7.data.execution.workflow.instructions

import cats.syntax.traverse.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.typeclasses.IsEmpty.syntax.ifEmpty
import js7.data.board.{BoardState, GlobalBoard, PlannableBoard}
import js7.data.controller.{ControllerEventDrivenStateView, ControllerState}
import js7.data.event.KeyedEvent
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderActorEvent, OrderNoticesExpected}
import js7.data.state.EngineState
import js7.data.workflow.instructions.{ConsumeNotices, ExpectNotices}

trait ConsumeOrExpectNoticesExecutor extends EventInstructionExecutor:

  type Instr <: ConsumeNotices | ExpectNotices

  final def toEvents(instr: Instr, order: Order[Order.State], state: EngineState)
  : Checked[List[KeyedEvent[OrderActorEvent]]] =
    detach(order)
      .orElse:
        start(order)
      .orElse:
        val controllerState = state.asInstanceOf[/*ControllerState*/ControllerEventDrivenStateView[?]]
        order.ifState[Order.Ready].map: order =>
          instr.referencedBoardPaths.toVector
            .traverse:
              state.keyTo(BoardState).checked
            .flatMap: boardStates =>
              boardStates.traverse: boardState =>
                boardState.board.match
                  case board: GlobalBoard =>
                    board.expectingOrderToNoticeId(order, state)
                      .left.map(_.withPrefix(s"${instr.getClass.shortClassName}:"))

                  case board: PlannableBoard =>
                    board.expectingOrderToNoticeId(order, state)
              .map: noticeIds =>
                instr.tryFulfill(order, noticeIds, controllerState.isNoticeAvailable)
                  .ifEmpty:
                    OrderNoticesExpected(noticeIds) :: Nil
                  .map(order.id <-: _)
      .orElse:
        val controllerState = state.asInstanceOf[ControllerState]
        order.ifState[Order.ExpectingNotices].map: order =>
          if order.state.noticeIds.map(_.boardPath).toSet != instr.referencedBoardPaths then
            Left(Problem.pure(s"${instr.getClass.shortClassName
              } instruction does not match Order.State: $instr <-> ${order.state}"))
          else
            Right:
              instr.tryFulfillExpectingOrder(order, controllerState.isNoticeAvailable)
                .map(order.id <-: _)
    .getOrElse:
      Right(Nil)
