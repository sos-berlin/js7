package js7.data.board

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.base.time.Timestamp
import js7.data.order.OrderEvent.OrderNoticesExpected
import org.jetbrains.annotations.TestOnly

final case class Notice(id: NoticeId, boardPath: BoardPath, endOfLife: Option[Timestamp])
extends NoticeSnapshot:

  def toExpected: OrderNoticesExpected.Expected =
    OrderNoticesExpected.Expected(boardPath, id)

  override def toString = s"Notice($boardPath $id${endOfLife.fold("")(o => s" $o")})"


object Notice:
  implicit val jsonCodec: Codec.AsObject[Notice] = deriveCodec

  @TestOnly
  def forPlannedBoard(plannedBoardId: PlannedBoardId): Notice =
    Notice(NoticeId.planned(plannedBoardId.planId), plannedBoardId.boardPath, endOfLife = None)
