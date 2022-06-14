package js7.base.time

import java.time.LocalTime.MIDNIGHT
import java.time.temporal.ChronoField.DAY_OF_WEEK
import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneOffset, Duration => JDuration}
import js7.base.time.JavaTime._
import js7.base.time.ScalaTime._
import scala.concurrent.duration._
import scala.jdk.DurationConverters.ScalaDurationOps

sealed trait AdmissionPeriodCalculator
{
  def admissionPeriod: AdmissionPeriod

  def dateOffset: JDuration

  def hasPeriodForDay(localDate: LocalDate): Boolean

  def toLocalInterval(local: LocalDateTime): Option[LocalInterval]

  protected def calendarPeriodStartWithoutDateOffset(local: LocalDateTime): LocalDateTime

  def nextCalendarPeriodStart(local: LocalDateTime): Option[LocalDateTime]
}

object AdmissionPeriodCalculator
{
  private[time] val NoOffset = ZoneOffset.ofTotalSeconds(0)
  private val JEpsilon = FiniteDuration.Epsilon.toJava

  def apply(admissionPeriod: AdmissionPeriod, dateOffset: FiniteDuration)
  : AdmissionPeriodCalculator =
    admissionPeriod match {
      case AlwaysPeriod =>
        AlwaysPeriodCalculator

      case period: DailyPeriod =>
        new DailyPeriodCalculator(period, dateOffset.toJava)

      case period: WeekdayPeriod =>
        new WeekdayPeriodCalculator(period, dateOffset.toJava)
    }

  private[time] case object AlwaysPeriodCalculator extends AdmissionPeriodCalculator
  {
    val admissionPeriod = AlwaysPeriod
    val dateOffset = JDuration.ZERO

    def hasPeriodForDay(localDate: LocalDate) =
      true

    def toLocalInterval(local: LocalDateTime) =
      Some(LocalInterval.Always)

    def calendarPeriodStartWithoutDateOffset(local: LocalDateTime) =
      LocalDateTime.MIN // Not used

    final def nextCalendarPeriodStart(local: LocalDateTime) =
      None
  }

  private[time] abstract class DayPeriodCalculator
  extends AdmissionPeriodCalculator
  {
    final def hasPeriodForDay(localDate: LocalDate) = {
      val startOfDay = LocalDateTime.of(localDate, MIDNIGHT)
      val endOfDay = startOfDay.plusDays(1)
      toLocalInterval0(startOfDay).contains(startOfDay, endOfDay)
    }

    final def toLocalInterval(local: LocalDateTime) =
      Some(toLocalInterval0(local))

    private def toLocalInterval0(local: LocalDateTime) = {
      val lastInterval = toLocalInterval1(calendarPeriodStart(local) - JEpsilon)
      if (lastInterval.contains(local))
        lastInterval
      else
        // Overlap from last calendar period — TODO Caller should check overlap
        toLocalInterval1(local)
    }

    private def toLocalInterval1(local: LocalDateTime) =
      LocalInterval(admissionPeriodStart(local), duration)

    /** The calendar period may be something like a whole day, week or month. */
    private[time] final def calendarPeriodStart(local: LocalDateTime): LocalDateTime =
      calendarPeriodStartWithoutDateOffset(local minus dateOffset) plus dateOffset

    protected def duration: FiniteDuration

    private[time] def admissionPeriodStart(local: LocalDateTime): LocalDateTime

    override def toString =
      admissionPeriod.toString
  }

  private[time] final class WeekdayPeriodCalculator(
    val admissionPeriod: WeekdayPeriod,
    val dateOffset: JDuration)
  extends DayPeriodCalculator
  {
    protected def duration =
      admissionPeriod.duration

    /** Monday 00:00 */
    def calendarPeriodStartWithoutDateOffset(local: LocalDateTime): LocalDateTime =
      LocalDateTime.of(
        local.toLocalDate.minusDays(local.get(DAY_OF_WEEK) - 1),
        MIDNIGHT)

    def nextCalendarPeriodStart(local: LocalDateTime) =
      Some(calendarPeriodStart(local).plusDays(7))

    private[time] def admissionPeriodStart(local: LocalDateTime) =
      LocalDateTime.ofEpochSecond(
        startOfWeek(local) + admissionPeriod.secondOfWeek,
        0,
        NoOffset)
  }

  private[time] def startOfWeek(local: LocalDateTime): Long =
    local.toEpochSecond(NoOffset) - sinceStartOfWeek(local.toEpochSecond(NoOffset))

  private[time] def sinceStartOfWeek(secondsSinceLocalEpoch: Long): Long = {
    val thursday = 3 // 1970-01-01 was a thursday
    (secondsSinceLocalEpoch + thursday * 24*3600) % (7*24*3600)
  }

  private[time] final class DailyPeriodCalculator(
    val admissionPeriod: DailyPeriod,
    val dateOffset: JDuration)
  extends DayPeriodCalculator
  {
    protected def duration = admissionPeriod.duration

    /** Same day, 00:00 */
    def calendarPeriodStartWithoutDateOffset(local: LocalDateTime): LocalDateTime =
      LocalDateTime.of(local.toLocalDate, MIDNIGHT)

    def nextCalendarPeriodStart(local: LocalDateTime) =
      Some(calendarPeriodStart(local) plusDays 1)

    private[time] def admissionPeriodStart(local: LocalDateTime) =
      LocalDateTime.of(
        local.toLocalDate,
        LocalTime.ofSecondOfDay(admissionPeriod.secondOfDay))
  }
}
