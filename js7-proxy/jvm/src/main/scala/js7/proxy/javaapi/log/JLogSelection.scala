package js7.proxy.javaapi.log

import java.time.Instant
import java.util.regex.Pattern
import java.util.{Optional, OptionalLong}
import js7.base.log.reader.LogSelection
import scala.jdk.OptionConverters.*

final case class JLogSelection(asScala: LogSelection = LogSelection.default):

  def end: Optional[Instant] =
    asScala.end.toJava

  def lineLimit: Optional[Long] =
    asScala.lineLimit.toJava

  def pattern: Optional[Pattern] =
    asScala.pattern.toJava

  def withEnd(end: Instant): JLogSelection =
    copy(asScala.copy(
      end = Some(end)))

  def withEnd(end: Optional[Instant]): JLogSelection =
    copy(asScala.copy(
      end = end.toScala))

  def withLineLimit(lineLimit: Long): JLogSelection =
    copy(asScala.copy(
      lineLimit = Some(lineLimit)))

  def withLineLimit(lineLimit: OptionalLong): JLogSelection =
    copy(asScala.copy(
      lineLimit = lineLimit.toScala))

  def withPattern(pattern: Pattern): JLogSelection =
    copy(asScala.copy(
      pattern = Some(pattern)))

  def withPattern(pattern: Optional[Pattern]): JLogSelection =
    copy(asScala.copy(
      pattern = pattern.toScala))

  def withByteChunkSize(byteChunkSize: Int): JLogSelection =
    copy(asScala.copy(
      byteChunkSize = byteChunkSize))

  def withGrowing(on: Boolean): JLogSelection =
    copy(asScala.copy(
      growing = on))


object JLogSelection:
  val empty: JLogSelection = new JLogSelection()
