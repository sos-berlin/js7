package js7.data.execution.workflow.instructions

import js7.data.board.NoticeEventSource
import js7.data.event.EventCalc
import js7.data.order.OrderEvent.OrderCoreEvent
import js7.data.order.{Order, OrderId}
import js7.data.state.EngineState_
import js7.data.workflow.instructions.PostNotices

private object PostNoticesExecutor extends EventInstructionExecutor_[PostNotices]:

  private val noticeEventSource = NoticeEventSource(forCommand = false)

  def toEventCalc[S <: EngineState_[S]](instr: PostNotices, orderId: OrderId)
  : EventCalc[S, OrderCoreEvent] =
    detachOrder(orderId): (coll, order) =>
      start(coll, orderId): (coll, order) =>
        order.ifState[Order.Ready].map: order =>
          coll:
            noticeEventSource.postNotices(instr.boardPaths, order)
        .getOrElse:
          coll.nix
