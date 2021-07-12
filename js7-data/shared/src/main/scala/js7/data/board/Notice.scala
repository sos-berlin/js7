package js7.data.board

import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.Big
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.order.OrderId
import js7.data.value.{ObjectValue, StringValue, Value}

sealed trait NoticeIdState
{
  def id: NoticeId
}

final case class Notice(id: NoticeId, endOfLife: Timestamp)
extends NoticeIdState

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
          //endOfLife <- obj.nameToValue
          //  .checked("endOfLife")
          //  .flatMap(_.asNumber)
          //  .flatMap(o => Checked.catchNonFatal(o.toBigInt.toLong))
          //  .map(Timestamp.ofEpochMilli)
        } yield Notice(id, endOfLife)

      case _ =>
        Left(Problem.pure("toNotice must return a String or an Object"))
    }

  implicit val jsonCodec = deriveCodec[Notice]
}


final case class AwaitingNotice(id: NoticeId, awaitingOrderIds: Seq[OrderId])
extends NoticeIdState with Big

object AwaitingNotice
{
  implicit val jsonCodec = deriveCodec[AwaitingNotice]  // TODO Big
}


object NoticeIdState
{
  implicit val jsonEncoder: Encoder.AsObject[NoticeIdState] = {
    case o: Notice => o.asJsonObject
    case o: AwaitingNotice => o.asJsonObject
  }

  implicit val jsonDecoder: Decoder[NoticeIdState] =
    c => if (c.keys.exists(_.exists(_ == "awaitingOrderIds")))
      c.value.as[AwaitingNotice]
    else
      c.value.as[Notice]
}
