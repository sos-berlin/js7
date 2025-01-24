package js7.data.execution.workflow.instructions

import js7.base.utils.ScalaUtils.syntax.RichJavaClass
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

  def toEvents(instr: PostNotices, order: Order[Order.State], state: StateView) =
    detach(order)
      .orElse:
        start(order)
      .orElse:
        order.ifState[Order.Ready].map: order =>
          noticeEventSource.postNotices(instr.boardPaths, order, state)
            .left.map(_.withPrefix(s"${instr.getClass.shortClassName}:"))
      .getOrElse:
        Right(Nil)
