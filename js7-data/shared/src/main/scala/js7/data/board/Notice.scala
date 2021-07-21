package js7.data.board

import io.circe.generic.semiauto.deriveCodec
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.Big
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.order.OrderId
import js7.data.value.{ObjectValue, StringValue, Value}

sealed trait NoticePlace
{
  def id: NoticeId
}

final case class Notice(id: NoticeId, endOfLife: Timestamp)
extends NoticePlace

object Notice
{
  def fromValue(value: Value, endOfLife: Timestamp): Checked[Notice] =
    value match {
      case StringValue(string) =>
        Right(Notice(NoticeId(string), endOfLife))

      case obj: ObjectValue =>
        for {
          id <- obj.nameToValue
            .checked("id")
            .flatMap(_.asString)
            .flatMap(NoticeId.checked)
        } yield Notice(id, endOfLife)

      case _ =>
        Left(Problem.pure("toNotice must return a String or an Object"))
    }


  final case class Snapshot(id: NoticeId, boardPath: BoardPath, endOfLife: Timestamp) {
    def notice = Notice(id, endOfLife)
  }

  object Snapshot {
    def apply(boardPath: BoardPath, notice: Notice) =
      new Snapshot(notice.id, boardPath, notice.endOfLife)
  }

  implicit val jsonCodec = deriveCodec[Notice]
  implicit val snapshotJsonCodec = deriveCodec[Notice.Snapshot]
}


final case class NoticeExpectation(id: NoticeId, awaitingOrderIds: Seq[OrderId])
extends NoticePlace with Big

object NoticeExpectation
{
  implicit val jsonCodec = deriveCodec[NoticeExpectation]  // TODO Big
}
