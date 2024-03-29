package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.derivation.ConfiguredCodec
import js7.data.board.BoardPathExpression
import js7.data.order.Order
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticesExpected, OrderNoticesRead}
import js7.data.source.SourcePos

final case class ExpectNotices(
  boardPaths: BoardPathExpression,
  sourcePos: Option[SourcePos] = None)
extends ExpectOrConsumeNoticesInstruction:

  def withoutSourcePos = copy(sourcePos = None)

  def referencedBoardPaths = boardPaths.boardPaths

  def fulfilledEvents(order: Order[Order.State], ignored: Vector[OrderNoticesExpected.Expected]) =
    OrderNoticesRead :: OrderMoved(order.position.increment) :: Nil


object ExpectNotices:
  implicit val jsonCodec: Codec.AsObject[ExpectNotices] = ConfiguredCodec.derive(useDefaults = true)
