package js7.proxy.javaapi.log

import java.time.Instant
import java.util.regex.Pattern
import java.util.{Optional, OptionalLong}
import js7.base.log.reader.LogSelection
import scala.jdk.OptionConverters.*

final case class JLogSelection(
  end: Optional[Instant] = Optional.empty,
  lineLimit: OptionalLong = OptionalLong.empty,
  pattern: Optional[Pattern] = Optional.empty,
  byteChunkSize: Int = LogSelection.default.byteChunkSize):

  def toScala: LogSelection =
    LogSelection(end.toScala, lineLimit.toScala, pattern.toScala)

  def withEnd(end: Instant): JLogSelection =
    copy(end = Optional.of(end))

  def withEnd(end: Optional[Instant]): JLogSelection =
    copy(end = end)

  def withLineLimit(lineLimit: Long): JLogSelection =
    copy(lineLimit = OptionalLong.of(lineLimit))

  def withLineLimit(lineLimit: OptionalLong): JLogSelection =
    copy(lineLimit = lineLimit)

  def withPattern(pattern: Pattern): JLogSelection =
    copy(pattern = Optional.of(pattern))

  def withPattern(pattern: Optional[Pattern]): JLogSelection =
    copy(pattern = pattern)

  def withByteChunkSize(byteChunkSize: Int): JLogSelection =
    copy(byteChunkSize = byteChunkSize)


object JLogSelection:
  val empty: JLogSelection = new JLogSelection()
