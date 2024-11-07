package js7.data.execution.workflow.instructions

import js7.data.board.NoticeEventSource
import js7.data.order.Order
import js7.data.state.StateView
import js7.data.workflow.instructions.PostNotices

private[instructions] final class PostNoticesExecutor(
  protected val service: InstructionExecutorService)
extends EventInstructionExecutor:

  type Instr = PostNotices
  val instructionClass = classOf[PostNotices]

  private val noticeEventSource = NoticeEventSource(clock)

  def toEvents(postNotices: PostNotices, order: Order[Order.State], state: StateView) =
    detach(order)
      .orElse:
        start(order)
      .orElse:
        order.ifState[Order.Ready].map: order =>
          noticeEventSource.postNotices(postNotices.boardPaths, order, state)
      .getOrElse:
        Right(Nil)
