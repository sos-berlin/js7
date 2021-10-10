package js7.base.time

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, OffsetDateTime, ZoneId, ZoneOffset, ZonedDateTime}
import js7.base.problem.Checked
import js7.base.time.JavaTimestamp.dateTimeFormatter
import js7.base.utils.ScalaUtils.syntax.RichAny
import org.jetbrains.annotations.TestOnly
import scala.jdk.CollectionConverters._

final case class JavaTimestamp private(toEpochMilli: Long) extends Timestamp
{
  import JavaTimestamp.specific._

  def companion = JavaTimestamp

  /** Returns an ISO-8601 string with milliseconds.
    * For example "2017-12-04T11:22:33.456Z".
    */
  def toIsoString = dateTimeFormatter.format(this.toInstant)

  def format(format: String, maybeTimezone: Option[String] = None): Checked[String] =
    Checked.catchNonFatal {
      val zone = maybeTimezone.fold(ZoneId.systemDefault())(ZoneId.of)
      this.toOffsetDateTime(zone).format(DateTimeFormatter.ofPattern(format))
    }

  def copy(epochMilli: Long): JavaTimestamp =
    JavaTimestamp.ofEpochMilli(epochMilli)
}

object JavaTimestamp extends Timestamp.Companion
{
  object specific {
    implicit class RichJavaTimestamp(private val timestamp: Timestamp) extends AnyVal
    {
      def toInstant = Instant.ofEpochMilli(timestamp.toEpochMilli)

      def toZonedDateTime(implicit zone: ZoneId): ZonedDateTime =
        ZonedDateTime.ofInstant(toInstant, zone)

      def toOffsetDateTime(implicit zone: ZoneId): OffsetDateTime =
        OffsetDateTime.ofInstant(toInstant, zone)

      def toLocalDateTime(implicit zone: ZoneId): LocalDateTime =
        LocalDateTime.ofInstant(toInstant, zone)

      def toJavaUtilDate: java.util.Date =
        new java.util.Date(timestamp.toEpochMilli)
    }
  }

  val MaxValue = ofEpochMilli(Long.MaxValue)

  def ofEpochMilli(o: Long) = new SystemTimestamp(o)

  def apply(instant: Instant) = ofInstant(instant)

  def apply(date: java.util.Date) = ofJavaUtilDate(date)

  def ofInstant(instant: Instant) = new SystemTimestamp(instant.toEpochMilli)

  def ofJavaUtilDate(date: java.util.Date) = JavaTimestamp.ofEpochMilli(date.getTime)

  private def dateTimeFormatter = DateTimeFormatter.ISO_INSTANT

  def parse(string: String): JavaTimestamp =
    ofInstant(Instant.from(dateTimeFormatter parse string))

  @TestOnly
  def local(string: String)(implicit zone: ZoneId): JavaTimestamp = {
    val local = LocalDateTime.parse(string)  // throws
    ofZoned(ZonedDateTime.of(local, zone))
  }

  /** Returns None for non-existant local time (due to dayligh saving time). */
  private def ofLocalExact(zoned: ZonedDateTime): Option[Timestamp] = {
    val local = zoned.toLocalDateTime
    zoned.getZone
      .match_ {
        case o: ZoneOffset => Some(o)
        case _ => zoned.getZone.getRules.getValidOffsets(local).asScala.headOption
      }
      .map(offset => Timestamp.ofEpochMilli(local.toInstant(offset).toEpochMilli))
  }

  def ofZoned(zoned: ZonedDateTime): JavaTimestamp =
    ofInstant(zoned.toInstant)
}
