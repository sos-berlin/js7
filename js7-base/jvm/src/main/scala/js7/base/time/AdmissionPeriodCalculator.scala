package js7.base.time

import java.time.LocalTime.MIDNIGHT
import java.time.temporal.ChronoField.DAY_OF_WEEK
import java.time.{LocalDate, LocalDateTime, ZoneOffset, Duration => JDuration}
import js7.base.time.JavaTime._
import js7.base.time.ScalaTime._
import scala.concurrent.duration._
import scala.jdk.DurationConverters.ScalaDurationOps

sealed trait AdmissionPeriodCalculator
{
  protected def dateOffset: JDuration

  def hasPeriodForDay(localDate: LocalDate): Boolean

  def toLocalInterval(local: LocalDateTime): Option[LocalInterval]

  private[time] def calendarStart(local: LocalDateTime): LocalDateTime =
    calendarStartWithoutDateOffset(local.minus(dateOffset))
      .plus(dateOffset)

  protected def calendarStartWithoutDateOffset(local: LocalDateTime): LocalDateTime

  final def nextCalendarStart(local: LocalDateTime) =
    calendarStart(local).plus(calendarDuration)

  def calendarDuration: JDuration
}

object AdmissionPeriodCalculator
{
  private val NoOffset = ZoneOffset.ofTotalSeconds(0)
  private val JEpsilon = FiniteDuration.Epsilon.toJava

  def apply(admissionPeriod: AdmissionPeriod, dateOffset: FiniteDuration)
  : AdmissionPeriodCalculator =
    admissionPeriod match {
      case AlwaysPeriod => AlwaysPeriodCalculator
      case period: DailyPeriod => new DailyPeriodCalculator(period, dateOffset.toJava)
      case period: WeekdayPeriod => new WeekdayPeriodCalculator(period, dateOffset.toJava)
    }

  private[time] case object AlwaysPeriodCalculator extends AdmissionPeriodCalculator
  {
    protected val dateOffset = JDuration.ZERO

    def hasPeriodForDay(localDate: LocalDate) =
      true

    def toLocalInterval(local: LocalDateTime) =
      Some(LocalInterval(local, FiniteDuration.MaxValue))

    def calendarStartWithoutDateOffset(local: LocalDateTime) =
      local  // not used

    def calendarDuration =
      FiniteDuration.MaxValue.toJava
  }

  private[time] abstract class DayPeriodCalculator
  extends AdmissionPeriodCalculator
  {
    final def hasPeriodForDay(localDate: LocalDate) = {
      val startOfDay = LocalDateTime.of(localDate, MIDNIGHT)
      val endOfDay = startOfDay.plusDays(1)
      val a = toLocalInterval0(startOfDay)
      a.end > startOfDay && a.start < endOfDay
    }

    final def toLocalInterval(local: LocalDateTime) =
      Some(toLocalInterval0(local))

    private def toLocalInterval0(local: LocalDateTime) = {
      val a = toLocalInterval1(calendarStart(local) - JEpsilon)  // Overlap from last week?
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

    protected def duration: FiniteDuration

    private[time] def secondsSinceStart(local: LocalDateTime): Long
  }

  private[time] final class WeekdayPeriodCalculator(
    weekdayPeriod: WeekdayPeriod,
    protected val dateOffset: JDuration)
  extends DayPeriodCalculator
  {
    protected def duration =
      weekdayPeriod.duration

    /** Monday 00:00 */
    def calendarStartWithoutDateOffset(local: LocalDateTime): LocalDateTime =
      LocalDateTime.of(
        local.toLocalDate.minusDays(local.get(DAY_OF_WEEK) - 1),
        MIDNIGHT)

    def calendarDuration = JDuration.ofDays(7)

    private[time] def secondsSinceStart(local: LocalDateTime): Long =
      (local.toEpochSecond(NoOffset) + 3/*1970-01-01 was a thursday*/ * 24 * 3600) % (7*24*3600) -
        weekdayPeriod.secondOfWeek

    override def toString =
      weekdayPeriod.toString
  }

  private[time] final class DailyPeriodCalculator(
    dailyPeriod: DailyPeriod,
    protected val dateOffset: JDuration)
  extends DayPeriodCalculator
  {
    protected def duration = dailyPeriod.duration
    import dailyPeriod.secondOfDay

    /** Same day, 00:00 */
    def calendarStartWithoutDateOffset(local: LocalDateTime): LocalDateTime =
      LocalDateTime.of(local.toLocalDate, MIDNIGHT)

    def calendarDuration = JDuration.ofDays(1)

    private[time] def secondsSinceStart(local: LocalDateTime): Long =
      local.toEpochSecond(NoOffset) % (24*3600) - secondOfDay

    override def toString =
      dailyPeriod.toString
  }
}
