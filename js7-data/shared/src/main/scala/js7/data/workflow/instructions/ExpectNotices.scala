package js7.data.workflow.instructions

import io.circe.derivation.ConfiguredCodec
import io.circe.{Codec, Decoder, Encoder}
import js7.base.utils.L3
import js7.data.board.{BoardPath, BoardPathExpression, NoticeId}
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticesRead}
import js7.data.order.{Order, OrderEvent}
import js7.data.source.SourcePos
import js7.data.workflow.Instruction
import js7.data.workflow.instructions.ExpectNotices.*
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction.WhenNotAnnounced
import js7.data.workflow.instructions.ExpectOrConsumeNoticesInstruction.WhenNotAnnounced.{DontWait, SkipWhenNoNotice, Wait}

final case class ExpectNotices(
  boardPaths: BoardPathExpression,
  whenNotAnnounced: WhenNotAnnounced = WhenNotAnnounced.Wait,
  sourcePos: Option[SourcePos] = None)
extends ExpectOrConsumeNoticesInstruction, Instruction.NoInstructionBlock:

  def withoutSourcePos: ExpectNotices =
    copy(sourcePos = None)

  def referencedBoardPaths: Set[BoardPath] =
    boardPaths.boardPaths

  protected def fulfilledEvents(
    order: Order[Order.Ready | Order.ExpectingNotices],
    noticeIds: Vector[NoticeId],
    consumedNoticeIds: Vector[NoticeId],
    exprResult: L3)
  : List[OrderNoticesRead | OrderMoved] =
    exprResult match
      case L3.False =>
        // Notices don't match but are announced
        Nil

      case L3.True =>
        // Notices match
        OrderNoticesRead :: OrderMoved(order.position.increment) :: Nil

      case L3.Unknown =>
        // Notices neither match nor are they announced
        whenNotAnnounced match
          case Wait => Nil
          case DontWait | SkipWhenNoNotice =>
            OrderNoticesRead
              :: OrderMoved(order.position.increment, Some(OrderMoved.NoNotice))
              :: Nil


object ExpectNotices:

  given Codec.AsObject[ExpectNotices] = ConfiguredCodec.derive(useDefaults = true)
