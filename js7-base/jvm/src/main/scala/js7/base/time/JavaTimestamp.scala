package js7.base.time

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, OffsetDateTime, ZoneId, ZoneOffset, ZonedDateTime}
import js7.base.problem.Checked
import js7.base.problem.Checked.catchExpected
import js7.base.time.JavaTimestamp.dateTimeFormatter
import org.jetbrains.annotations.TestOnly
import scala.jdk.CollectionConverters.*

final case class JavaTimestamp private(toEpochMilli: Long) extends Timestamp:
  import JavaTimestamp.specific.*

  protected type Self = JavaTimestamp

  def companion: Timestamp.Companion =
    JavaTimestamp

  /** Returns an ISO-8601 string with milliseconds.
    * For example "2017-12-04T11:22:33.456Z".
    */
  def toIsoString: String =
    dateTimeFormatter.format(this.toInstant)

  def format(format: String, maybeTimezone: Option[String] = None): Checked[String] =
    catchExpected[Exception]:
      val zone = maybeTimezone.fold(ZoneId.systemDefault())(ZoneId.of)
      this.toOffsetDateTime(zone).format(DateTimeFormatter.ofPattern(format))

  def copy(epochMilli: Long): Timestamp =
    JavaTimestamp.ofEpochMilli(epochMilli)


object JavaTimestamp extends Timestamp.Companion:
  object specific:
    implicit class RichJavaTimestamp(private val timestamp: Timestamp) extends AnyVal:
      def toInstant: Instant =
        Instant.ofEpochMilli(timestamp.toEpochMilli)

      def toZonedDateTime(implicit zone: ZoneId): ZonedDateTime =
        ZonedDateTime.ofInstant(toInstant, zone)

      def toOffsetDateTime(implicit zone: ZoneId): OffsetDateTime =
        OffsetDateTime.ofInstant(toInstant, zone)

      def toLocalDateTime(implicit zone: ZoneId): LocalDateTime =
        LocalDateTime.ofInstant(toInstant, zone)

      def toJavaUtilDate: java.util.Date =
        new java.util.Date(timestamp.toEpochMilli)

    implicit class RichJavaTimestampCompanion(private val x: Timestamp.type) extends AnyVal:
      def fromJavaUtilDate(date: java.util.Date): Timestamp =
        Timestamp.ofEpochMilli(date.getTime)

  val MaxValue: Timestamp =
    ofEpochMilli(Long.MaxValue)

  def ofEpochMilli(o: Long): Timestamp =
    new JavaTimestamp(o)

  def apply(instant: Instant): Timestamp =
    ofInstant(instant)

  def apply(date: java.util.Date): Timestamp =
    ofJavaUtilDate(date)

  def ofInstant(instant: Instant): JavaTimestamp =
    new JavaTimestamp(instant.toEpochMilli)

  def ofJavaUtilDate(date: java.util.Date): Timestamp =
    JavaTimestamp.ofEpochMilli(date.getTime)

  private def dateTimeFormatter =
    DateTimeFormatter.ISO_INSTANT

  def parse(string: String): Timestamp =
    ofInstant(Instant.from(dateTimeFormatter parse string))

  @TestOnly
  def local(string: String)(implicit zone: ZoneId): JavaTimestamp =
    val local = LocalDateTime.parse(string)  // throws
    ofZoned(ZonedDateTime.of(local, zone))

  /** Returns None for non-existant local time (due to dayligh saving time). */
  private def ofLocalExact(zoned: ZonedDateTime): Option[Timestamp] =
    val local = zoned.toLocalDateTime
    zoned.getZone
      .match
        case o: ZoneOffset => Some(o)
        case _ => zoned.getZone.getRules.getValidOffsets(local).asScala.headOption
      .map(offset => Timestamp.ofEpochMilli(local.toInstant(offset).toEpochMilli))

  def ofZoned(zoned: ZonedDateTime): JavaTimestamp =
    ofInstant(zoned.toInstant)
