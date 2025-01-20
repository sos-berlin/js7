package js7.data.board

import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Decoder, Encoder, Json}
import js7.base.time.Timestamp
import js7.data.order.OrderEvent.OrderNoticesExpected
import org.jetbrains.annotations.TestOnly

final case class Notice(id: NoticeId, endOfLife: Option[Timestamp])
extends NoticeSnapshot:

  export id.{boardPath, noticeKey, planId, plannedNoticeKey, boardNoticeKey}

  def toExpected: OrderNoticesExpected.Expected =
    OrderNoticesExpected.Expected(id.boardNoticeKey)

  override def toString = s"Notice($id${endOfLife.fold("")(o => s" $o")})"


object Notice:
  private val jsonCodec: Codec.AsObject[Notice] = deriveCodec

  given Encoder.AsObject[Notice] = jsonCodec

  given Decoder[Notice] = c =>
    if c.get[Json]("boardPath").isRight then
      // COMPATIBLE with v2.7.3
      for
        boardPath <- c.get[BoardPath]("boardPath")
        plannedNoticeKey <- c.get[PlannedNoticeKey]("id")
        endOfLife <- c.get[Option[Timestamp]]("endOfLife")
      yield
        Notice(boardPath / plannedNoticeKey, endOfLife)
    else
      jsonCodec(c)

  def apply(id: NoticeId, endOfLife: Option[Timestamp] = None): Notice =
    new Notice(id, endOfLife)

  @TestOnly
  def forPlannedBoard(plannedBoardId: PlannedBoardId): Notice =
    Notice(
      plannedBoardId.boardPath / PlannedNoticeKey.empty(plannedBoardId.planId),
      endOfLife = None)
