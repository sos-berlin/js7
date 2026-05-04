package js7.base.log.reader

import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}
import java.time.Instant
import js7.base.circeutils.CirceUtils.toDecoderResult
import js7.base.circeutils.JavaDataJsonCodecs.instant.NumericInstantJsonCodec
import js7.base.log.{AnsiEscapeCodes, LogLevel}
import js7.base.problem.{Checked, Problem}

final case class KeyedLogLine(key: LogLineKey, line: String):
  export key.{fileInstant, position}

  def removeHighlights: KeyedLogLine =
    copy(line = AnsiEscapeCodes.removeHighlights(line))

  def asString: String =
    s"${key.asString} $line"


object KeyedLogLine:
  def apply(logLevel: LogLevel, fileInstant: Instant, position: Long, line: String): KeyedLogLine =
    KeyedLogLine(LogLineKey(logLevel, fileInstant, position), line)

  def parse(string: String): Checked[KeyedLogLine] =
    string.split(" ", 2) match
      case Array(keyString, line) =>
        LogLineKey.parse(keyString).map(KeyedLogLine(_, line))
      case _ =>
        Left(Problem("Invalid KeyedLogLine format"))

  def parse(byteLine: fs2.Chunk[Byte]): Checked[KeyedLogLine] =
    KeyedByteLogLine.parse(byteLine).map(_.toKeyedLogLine)


  given Codec[Instant] = NumericInstantJsonCodec

  /** Not used */
  private[reader] given Encoder[KeyedLogLine] = o =>
    Json.arr(o.key.asString.asJson, o.line.asJson)

  /** Not used */
  private[reader] given Decoder[KeyedLogLine] = c =>
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
