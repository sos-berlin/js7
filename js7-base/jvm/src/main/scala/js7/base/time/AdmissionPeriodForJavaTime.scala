package js7.base.time

import java.time.temporal.ChronoField.DAY_OF_WEEK
import java.time.{LocalDateTime, LocalTime, ZoneOffset}
import js7.base.time.JavaTime._
import js7.base.time.ScalaTime._
import scala.concurrent.duration._

/** Code requiring java time (JVM) .*/
object AdmissionPeriodForJavaTime
{
  implicit final class JavaAdmissionPeriod(private val admissionPeriod: AdmissionPeriod)
  extends AnyVal
  {
    def toInterval(local: LocalDateTime): Option[LocalInterval] =
      toJava(admissionPeriod)
        .toLocalInterval(local)

    /** For example, the start of the week for a WeekdayPeriod. */
    def calendarStart(local: LocalDateTime): LocalDateTime =
      toJava(admissionPeriod)
        .calendarStart(local)

    /** For example, the start of the next week for a WeekdayPeriod. */
    def nextCalendarStart(local: LocalDateTime): LocalDateTime =
      toJava(admissionPeriod)
        .nextCalendarStart(local)
  }

  private def toJava(admissionPeriod: AdmissionPeriod): AdmissionPeriodJava =
    admissionPeriod match {
      case AlwaysPeriod => JavaAlwaysPeriod
      case o: WeekdayPeriod => JavaWeekdayPeriod(o)
    }

  private val NoOffset = ZoneOffset.ofTotalSeconds(0)

  private sealed trait AdmissionPeriodJava
  {
    def toLocalInterval(local: LocalDateTime): Option[LocalInterval]

    def calendarStart(local: LocalDateTime): LocalDateTime

    def nextCalendarStart(local: LocalDateTime): LocalDateTime
  }

  private case object JavaAlwaysPeriod extends AdmissionPeriodJava
  {
    def toLocalInterval(local: LocalDateTime) =
      Some(LocalInterval(local, FiniteDuration.MaxValue))

    def calendarStart(local: LocalDateTime) =
      local  // not used

    def nextCalendarStart(local: LocalDateTime) =
      local  // not used
  }

  private final case class JavaWeekdayPeriod(weekdayPeriod: WeekdayPeriod)
  extends AdmissionPeriodJava
  {
    import weekdayPeriod.{duration, secondOfWeek}

    def toLocalInterval(local: LocalDateTime) = {
      val a = toInterval1(calendarStart(local) - 1.ns)  // Overlap from last week?
      Some(
        if (a.contains(local))
          a
        else
          // TODO Caller should check overlap
          toInterval1(local))
    }

    private def toInterval1(local: LocalDateTime) = {
      val s = secondsSinceStartOfWeek(local.toEpochSecond(NoOffset))
      LocalInterval(local.withNano(0).plusSeconds(s), duration)
    }

    def calendarStart(local: LocalDateTime): LocalDateTime =
      LocalDateTime.of(
        local.toLocalDate.minusDays(local.toLocalDate.get(DAY_OF_WEEK) - 1),
        LocalTime.MIDNIGHT)

    def nextCalendarStart(local: LocalDateTime): LocalDateTime =
      LocalDateTime.of(
        local.toLocalDate.plusDays(8 - local.toLocalDate.get(DAY_OF_WEEK)),
        LocalTime.MIDNIGHT)

    private def secondsSinceStartOfWeek(second: Long): Long =
      secondOfWeek -
        ((second + 3/*1970-01-01 was a thursday*/ * 24 * 3600) % (7*24*3600))

    override def toString =
      weekdayPeriod.toString
  }
}
