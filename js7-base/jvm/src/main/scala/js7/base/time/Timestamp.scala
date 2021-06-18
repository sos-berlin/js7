package js7.base.time

import java.time.format.DateTimeFormatter
import java.time.{Instant, OffsetDateTime, ZoneId}
import js7.base.problem.Checked
import js7.base.time.Timestamp.dateTimeFormatter

/**
  * @author Joacim Zschimmer
  */
final case class Timestamp private(toEpochMilli: Long) extends GenericTimestamp[Timestamp]
{
  def companion = Timestamp

  /** Returns an ISO-8601 string with milliseconds.
    * For example "2017-12-04T11:22:33.456Z".
    */
  def toIsoString = dateTimeFormatter.format(toInstant)

  def toInstant = Instant.ofEpochMilli(toEpochMilli)

  def toJavaUtilDate: java.util.Date =
    new java.util.Date(toEpochMilli)

  def format(format: String, maybeTimezone: Option[String] = None): Checked[String] =
    Checked.catchNonFatal {
      val zoneId = maybeTimezone.fold(ZoneId.systemDefault())(ZoneId.of)
      OffsetDateTime.ofInstant(toInstant, zoneId).format(DateTimeFormatter.ofPattern(format))
    }

  def copy(epochMilli: Long): Timestamp =
    Timestamp.ofEpochMilli(epochMilli)
}

object Timestamp extends GenericTimestamp.Companion[Timestamp]
{
  val MaxValue = ofEpochMilli(Long.MaxValue)
  private val UTC = ZoneId.of("UTC")

  // Hide IntelliJ red underlines here
  override def apply(string: String): Timestamp =
    super.apply(string)

  def ofEpochMilli(o: Long) = new Timestamp(o)

  def apply(instant: Instant) = ofInstant(instant)

  def apply(date: java.util.Date) = ofJavaUtilDate(date)

  def ofInstant(instant: Instant) = new Timestamp(instant.toEpochMilli)

  def ofJavaUtilDate(date: java.util.Date) = Timestamp.ofEpochMilli(date.getTime)

  private def dateTimeFormatter = DateTimeFormatter.ISO_INSTANT

  def parse(string: String): Timestamp =
    ofInstant(Instant.from(dateTimeFormatter parse string))
}
