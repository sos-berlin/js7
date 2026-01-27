package js7.data.execution.workflow.instructions

import cats.syntax.traverse.*
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.board.{BoardPath, BoardState, GlobalBoard, NoticeEventSource, PlannableBoard}
import js7.data.controller.ControllerState
import js7.data.event.{EventCalc, KeyedEvent}
import js7.data.order.Order.ExpectingNotices
import js7.data.order.OrderEvent.{OrderCoreEvent, OrderNoticesExpected}
import js7.data.order.{Order, OrderId}
import js7.data.state.EngineState_
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction
import scala.reflect.ClassTag

trait ConsumeOrExpectNoticesExecutor extends EventInstructionExecutor:

  type Instr <: ExpectOrConsumeNoticesInstruction

  private val noticeEventSource = NoticeEventSource(forCommand = false)

  def toEventCalc[S <: EngineState_[S]](instr: Instr, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    detachOrder(orderId): (coll, order) =>
      start(coll, orderId): (coll, order) =>
        for
          coll <- coll.narrowAggregate[ControllerState]
          coll <-
            order.ifState[Order.Ready].map: order =>
              instr.referencedBoardPaths.toVector.traverse:
                coll.aggregate.keyTo(BoardState).checked
              .flatMap: boardStates =>
                boardStates.traverse: boardState =>
                  boardState.board.match
                    case board: GlobalBoard =>
                      board.expectingOrderToNoticeId(order, coll.aggregate)
                        .left.map(_.withPrefix(s"${instr.getClass.shortClassName}:"))

                    case board: PlannableBoard =>
                      board.expectingOrderToNoticeId(order, coll.aggregate)
                .flatMap: noticeIds =>
                  noticeEventSource
                    .tryFulfill(instr, order, noticeIds, coll.aggregate.isNoticeAvailable)
                    .widen.ifHasEventsAddToCollElse(coll):
                      coll:
                        order.id <-: OrderNoticesExpected(noticeIds)
            .orElse:
              order.ifState[Order.ExpectingNotices].map: order =>
                if order.state.noticeIds.map(_.boardPath).toSet != instr.referencedBoardPaths then
                  Left(Problem.pure(s"${instr.getClass.shortClassName
                    } instruction does not match Order.State: $instr <-> ${order.state}"))
                else
                  coll:
                    noticeEventSource.
                      tryFulfillExpectingOrder(instr, order, coll.aggregate.isNoticeAvailable)
            .getOrElse:
              coll.nix
        yield coll
