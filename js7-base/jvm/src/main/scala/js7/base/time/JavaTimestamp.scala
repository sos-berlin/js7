package js7.base.time

import java.time.format.DateTimeFormatter
import java.time.{Instant, OffsetDateTime, ZoneId}
import js7.base.problem.Checked
import js7.base.time.JavaTimestamp.dateTimeFormatter

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
      val zoneId = maybeTimezone.fold(ZoneId.systemDefault())(ZoneId.of)
      OffsetDateTime.ofInstant(this.toInstant, zoneId).format(DateTimeFormatter.ofPattern(format))
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

      def toJavaUtilDate: java.util.Date =
        new java.util.Date(timestamp.toEpochMilli)

    }
  }

  val MaxValue = ofEpochMilli(Long.MaxValue)
  private val UTC = ZoneId.of("UTC")

  def ofEpochMilli(o: Long) = new SystemTimestamp(o)

  def apply(instant: Instant) = ofInstant(instant)

  def apply(date: java.util.Date) = ofJavaUtilDate(date)

  def ofInstant(instant: Instant) = new SystemTimestamp(instant.toEpochMilli)

  def ofJavaUtilDate(date: java.util.Date) = JavaTimestamp.ofEpochMilli(date.getTime)

  private def dateTimeFormatter = DateTimeFormatter.ISO_INSTANT

  def parse(string: String): JavaTimestamp =
    ofInstant(Instant.from(dateTimeFormatter parse string))
}
