package js7.base.log.reader

import java.time.Instant
import java.util.regex.Pattern
import js7.base.io.file.ByteSeqFileReader
import js7.base.utils.ScalaUtils.flatten

final case class LogSelection(
  end: Option[Instant] = None,
  lineLimit: Option[Long] = None,
  pattern: Option[Pattern] = None,
  byteChunkSize: Int = ByteSeqFileReader.BufferSize,
  growing: Boolean = false):

  def toKeyValues: Seq[(String, String)] =
    flatten(
      end.map(o => "end" -> o.toString),
      lineLimit.map(o => "lineLimit" -> o.toString),
      pattern.map(o => "pattern" -> o.pattern))


object LogSelection:
  val default: LogSelection =
    new LogSelection()

  def apply(): LogSelection =
    default
