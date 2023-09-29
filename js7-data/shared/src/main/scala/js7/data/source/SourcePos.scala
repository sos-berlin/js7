package js7.data.source

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

/** `start` and `end` are character positions starting with 0. */
final case class SourcePos(start: Int, end: Int):
  require(start <= end, s"Invalid SourcePos($start, $end)")

object SourcePos:
  implicit val jsonEncoder: Encoder[SourcePos] =
    o => Json.fromValues(Array(o.start.asJson, o.end.asJson))

  implicit val jsonDecoder: Decoder[SourcePos] =
    c => c.value.as[Array[Int]].flatMap:
      case a if a.length == 2 => Right(SourcePos(a(0), a(1)))
      case _ => Left(DecodingFailure("Not a SourcePos", c.history))
