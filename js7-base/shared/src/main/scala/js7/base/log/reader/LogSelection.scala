package js7.base.log.reader

import java.time.Instant
import java.util.regex.Pattern
import js7.base.io.file.ByteSeqFileReader
import js7.base.log.reader.LogSelection.*
import js7.base.utils.ScalaUtils.flatten
import js7.base.utils.ScalaUtils.syntax.*

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
      pattern.map(o => "pattern" -> o.pattern),
      growing ? ("growing" -> "true"))

  def forReader: ForReader =
    ForReader(growing, byteChunkSize)


object LogSelection:
  val default: LogSelection =
    new LogSelection()

  def apply(): LogSelection =
    default

  final case class ForReader(growing: Boolean = false, byteChunkSize: Int)
