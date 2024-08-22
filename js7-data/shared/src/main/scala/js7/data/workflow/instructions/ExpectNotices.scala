package js7.data.workflow.instructions

import io.circe.Codec
import io.circe.derivation.ConfiguredCodec
import js7.data.board.{BoardPath, BoardPathExpression}
import js7.data.order.OrderEvent.{OrderMoved, OrderNoticesExpected, OrderNoticesRead}
import js7.data.order.{Order, OrderEvent}
import js7.data.source.SourcePos
import scala.annotation.unused

final case class ExpectNotices(
  boardPaths: BoardPathExpression,
  sourcePos: Option[SourcePos] = None)
extends ExpectOrConsumeNoticesInstruction:

  def withoutSourcePos: ExpectNotices =
    copy(sourcePos = None)

  def referencedBoardPaths: Set[BoardPath] =
    boardPaths.boardPaths

  def fulfilledEvents(
    order: Order[Order.Ready | Order.ExpectingNotices],
    @unused expected: Vector[OrderNoticesExpected.Expected])
  : List[OrderEvent.OrderActorEvent] =
    OrderNoticesRead :: OrderMoved(order.position.increment) :: Nil


object ExpectNotices:
  implicit val jsonCodec: Codec.AsObject[ExpectNotices] = ConfiguredCodec.derive(useDefaults = true)
