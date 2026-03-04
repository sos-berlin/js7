package js7.base.log.reader

import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}
import java.time.Instant
import js7.base.circeutils.CirceUtils.toDecoderResult
import js7.base.circeutils.JavaDataJsonCodecs.instant.NumericInstantJsonCodec
import js7.base.log.{AnsiEscapeCodes, LogLevel}

final case class KeyedLogLine(key: LogLineKey, line: String):
  export key.{fileInstant, position}

  def removeHighlights: KeyedLogLine =
    copy(line = AnsiEscapeCodes.removeHighlights(line))


object KeyedLogLine:
  def apply(logLevel: LogLevel, instant: Instant, position: Long, line: String): KeyedLogLine =
    KeyedLogLine(LogLineKey(logLevel, instant, position), line)

  given Codec[Instant] = NumericInstantJsonCodec

  given Encoder[KeyedLogLine] = o =>
    Json.arr(o.key.asString.asJson, o.line.asJson)

  given Decoder[KeyedLogLine] = c =>
    c.values match
      case None => Left(DecodingFailure("Array expected", c.history))
      case Some(iterable) =>
        val array = iterable.toIndexedSeq
        if array.size != 2 then
          Left(DecodingFailure(s"Array of size 3 expected, got ${array.size}", c.history))
        else
          for
            logLineKey <- LogLineKey.parse(array(0).asString.getOrElse(""))
              .toDecoderResult(c.history)
            line <- array(1).as[String]
          yield
            KeyedLogLine(logLineKey, line)
