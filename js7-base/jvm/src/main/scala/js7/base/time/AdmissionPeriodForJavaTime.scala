package js7.base.time

import java.time.LocalTime.MIDNIGHT
import java.time.temporal.ChronoField.DAY_OF_WEEK
import java.time.{LocalDate, LocalDateTime, ZoneOffset}
import js7.base.time.JavaTime._
import js7.base.time.ScalaTime._
import scala.concurrent.duration._

/** Code requiring java time (JVM) .*/
object AdmissionPeriodForJavaTime
{
  implicit final class JavaAdmissionPeriod(private val admissionPeriod: AdmissionPeriod)
  extends AnyVal
  {
    def hasPeriodForDay(localDate: LocalDate): Boolean =
      toJava(admissionPeriod)
        .hasPeriodForDay(localDate)

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

  private[time] sealed trait AdmissionPeriodJava
  {
    def hasPeriodForDay(localDate: LocalDate): Boolean

    def toLocalInterval(local: LocalDateTime): Option[LocalInterval]

    def calendarStart(local: LocalDateTime): LocalDateTime

    def nextCalendarStart(local: LocalDateTime): LocalDateTime
  }

  private case object JavaAlwaysPeriod extends AdmissionPeriodJava
  {
    def hasPeriodForDay(localDate: LocalDate) =
      true

    def toLocalInterval(local: LocalDateTime) =
      Some(LocalInterval(local, FiniteDuration.MaxValue))

    def calendarStart(local: LocalDateTime) =
      local  // not used

    def nextCalendarStart(local: LocalDateTime) =
      local  // not used
  }

  private[time] final case class JavaWeekdayPeriod(weekdayPeriod: WeekdayPeriod)
  extends AdmissionPeriodJava
  {
    import weekdayPeriod.{duration, secondOfWeek}

    def hasPeriodForDay(localDate: LocalDate) = {
      val startOfDay = LocalDateTime.of(localDate, MIDNIGHT)
      val endOfDay = startOfDay.plusDays(1)
      val a = toLocalInterval0(startOfDay)
      a.end > startOfDay && a.start < endOfDay
    }

    def toLocalInterval(local: LocalDateTime) =
      Some(toLocalInterval0(local))

    def toLocalInterval0(local: LocalDateTime) = {
      val a = toLocalInterval1(calendarStart(local) - FiniteDuration.Epsilon)  // Overlap from last week?
      if (a.contains(local))
        a
      else
        // TODO Caller should check overlap
        toLocalInterval1(local)
    }

    private def toLocalInterval1(local: LocalDateTime) =
      LocalInterval(
        local.withNano(0).minusSeconds(secondsSinceStart(local)),
        duration)

    def calendarStart(local: LocalDateTime): LocalDateTime =
      LocalDateTime.of(
        local.toLocalDate.minusDays(local.toLocalDate.get(DAY_OF_WEEK) - 1),
        MIDNIGHT)

    def nextCalendarStart(local: LocalDateTime): LocalDateTime =
      LocalDateTime.of(
        local.toLocalDate.plusDays(8 - local.toLocalDate.get(DAY_OF_WEEK)),
        MIDNIGHT)

    private[time] def secondsSinceStart(local: LocalDateTime): Long =
      (local.toEpochSecond(NoOffset) + 3/*1970-01-01 was a thursday*/ * 24 * 3600) % (7*24*3600) -
        secondOfWeek

    override def toString =
      weekdayPeriod.toString
  }
}
