package js7.proxy.javaapi.log

import java.time.Instant
import java.util.regex.Pattern
import java.util.{Optional, OptionalLong}
import js7.base.log.reader.LogSelection
import scala.jdk.OptionConverters.*

final case class JLogSelection(asScala: LogSelection = LogSelection.all):

  def end: Optional[Instant] =
    asScala.end.toJava

  def lineLimit: Optional[Long] =
    asScala.lineLimit.toJava

  def pattern: Optional[Pattern] =
    asScala.pattern.toJava

  /** Select only lines until before the given end timestamp. */
  def withEnd(end: Instant): JLogSelection =
    copy(asScala.copy(
      end = Some(end)))

  /** Select only lines until before the given end timestamp.
    *
    * Optional.empty() means no end timestamp. */
  def withEnd(end: Optional[Instant]): JLogSelection =
    copy(asScala.copy(
      end = end.toScala))

  /** Limit the number of lines. */
  def withLineLimit(lineLimit: Long): JLogSelection =
    copy(asScala.copy(
      lineLimit = Some(lineLimit)))

  /** Limit the number of lines.
    *
    * OptionalLong.empty means no line limit. */
  def withLineLimit(lineLimit: OptionalLong): JLogSelection =
    copy(asScala.copy(
      lineLimit = lineLimit.toScala))

  /** Select only lines which match the given pattern.
    *
    * - Lines don't terminate with '\n'. Use $ for end of line
    * - Some ANSI escape sequences at start and end of line are removed before matching.
    * - See [[LogSelection.tailorRegion]]
    */
  def withPattern(pattern: Pattern) =
    copy(asScala.copy(
      pattern = Some(pattern)))

  def withPattern(pattern: Optional[Pattern]): JLogSelection =
    copy(asScala.copy(
      pattern = pattern.toScala))

  /** The result stream should grow endlessly as the log files grow.
    *
    * The caller has to cancel the stream, when no other limit has been set. */
  def withGrowing(on: Boolean): JLogSelection =
    copy(asScala.copy(
      growing = on))

  /** Use only if you know what you are doing.
    *
    * Does not survive HTTP transfer (i.e, JS7 Engine log files). */
  def withByteChunkSize(byteChunkSize: Int): JLogSelection =
    copy(asScala.copy(
      byteChunkSize = byteChunkSize))


object JLogSelection:
  /** Select all lines (but no future lines — don't grow endlessly).
    *
    * The selection may be restricted by JSelection's with-methods.
    */
  val all: JLogSelection = new JLogSelection()

  val empty: JLogSelection = all
