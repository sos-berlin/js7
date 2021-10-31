package js7.provider.scheduledorder.oldruntime

import java.time.{Duration, LocalDate, LocalDateTime, LocalTime, ZoneId}
import js7.base.convert.As
import js7.provider.scheduledorder.oldruntime.ExtendedLocalTime._
import scala.language.implicitConversions

/**
  * Like LocalTime, but may span several days, with 24 hours a day.
  *
  * @author Joacim Zschimmer
  */
sealed abstract case class ExtendedLocalTime(toNanoOfDay: Long)
extends Ordered[ExtendedLocalTime]
{
  require(toNanoOfDay >= 0, "ExtendedLocalTime must not be negative")

  def +(o: Duration): ExtendedLocalTime =
    ExtendedLocalTime.ofNanoOfDay(toNanoOfDay + o.toNanos)

  def atDate(date: LocalDate): LocalDateTime =
    LocalDateTime.of(date plusDays days, localTime)

  def compare(o: ExtendedLocalTime) =
    toNanoOfDay compare o.toNanoOfDay

  def toInstant(localDate: LocalDate, zone: ZoneId) = {
    val local = toLocalDateTime(localDate)
    local.toInstant(zone.getRules.getOffset(local))
  }

  def toLocalDateTime(localDate: LocalDate) =
    LocalDateTime.of(localDate plusDays days, localTime)

  def localTime =
    LocalTime.ofNanoOfDay(toNanoOfDay % (24*60*60*Billion))

  def days: Long =
    toNanoOfDay / (24*60*60*Billion)

  override def toString = {
    val h = toNanoOfDay / (60*60*Billion)
    val m = toNanoOfDay / (60*Billion) % 60
    val s = toNanoOfDay / Billion % 60
    val n = toNanoOfDay % Billion
    val sb = new StringBuilder
    sb.append(h)
    sb.append(':')
    if (m < 10) sb.append('0')
    sb.append(m)
    sb.append(':')
    if (s < 10) sb.append('0')
    sb.append(s)
    if (n != 0) {
      sb.append('.')
      sb.append(n)
    }
    sb.toString
  }
}

object ExtendedLocalTime {
  private val Billion = 1000*1000*1000L
  val StartOfDay = of(0, 0)
  val EndOfDay = of(24, 0)
  implicit val stringAs: As[String, ExtendedLocalTime] = As(fromString)
  private val ParseRegex = """([0-9]+):([0-9]{2})(?::([0-9]{2}))?""".r

  implicit def fromLocalTime(localTime: LocalTime): ExtendedLocalTime =
    ExtendedLocalTime.ofNanoOfDay(localTime.toNanoOfDay)

  def fromString(o: String): ExtendedLocalTime =
    o match {
      case ParseRegex(hours, minutes, seconds) =>
        of(hours.toInt, minutes.toInt, (Option(seconds) getOrElse "0").toInt)
      case _ => throw new IllegalArgumentException(s"Not a local time: '$o'")
  }

  def of(hour: Int, minute: Int, second: Int = 0): ExtendedLocalTime = {
    require(hour >= 0 && minute >= 0 && second >= 0, "Value must not be negative")
    require(minute < 60, "Minute must be less than 60")
    require(second < 60, "Secondmust be less than 60")
    ofSecondOfDay(hour * 60*60 + minute * 60 + second)
  }

  def ofSecondOfDay(seconds: Int): ExtendedLocalTime =
    new ExtendedLocalTime(seconds * Billion) {}

  def ofNanoOfDay(nanos: Long): ExtendedLocalTime =
    new ExtendedLocalTime(nanos) {}
}
