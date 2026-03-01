package js7.base.log

import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}
import java.time.Instant
import js7.base.circeutils.JavaDataJsonCodecs.instant.NumericInstantJsonCodec
import js7.base.time.JavaTimeExtensions.toEpochNano

final case class KeyedLogLine(key: LogLineKey, line: String):
  export key.{instant, position}


object KeyedLogLine:
  def apply(instant: Instant, position: Long, line: String): KeyedLogLine =
    KeyedLogLine(LogLineKey(instant, position), line)

  given Codec[Instant] = NumericInstantJsonCodec

  given Encoder[KeyedLogLine] = o =>
    Json.arr(o.instant.toEpochNano.asJson, o.position.asJson, o.line.asJson)

  given Decoder[KeyedLogLine] = c =>
    c.values match
      case None => Left(DecodingFailure("Array expected", c.history))
      case Some(iterable) =>
        val array = iterable.toIndexedSeq
        if array.size != 3 then
          Left(DecodingFailure(s"Array of size 3 expected, got ${array.size}", c.history))
        else
          for
            instant <- array(0).as[Instant]
            pos <- array(1).as[Long]
            line <- array(2).as[String]
          yield
            KeyedLogLine(LogLineKey(instant, pos), line)
