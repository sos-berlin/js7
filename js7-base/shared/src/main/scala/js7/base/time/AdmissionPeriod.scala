package js7.base.time

import io.circe.generic.semiauto.deriveCodec
import java.time.{DayOfWeek, LocalTime}
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.{Checked, Problem}
import js7.base.time.AdmissionPeriod._
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.syntax._
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration._

/** Periodical admission time expressed in local time. */
sealed trait AdmissionPeriod

case object AlwaysPeriod extends AdmissionPeriod

/** Weekly admission time. */
final case class WeekdayPeriod(secondOfWeek: Int, duration: FiniteDuration)
extends AdmissionPeriod
{
  assertThat(secondOfWeek >= 0 && secondOfWeek <= WeekSeconds)
  assert(duration <= WeekDuration)

  def checked: Checked[this.type] =
    if (secondOfWeek < 0 || secondOfWeek >= WeekSeconds)
      Left(Problem(s"Invalid weekday time number: $toString"))
    else if (!duration.isPositive || duration > WeekDuration)
      Left(Problem(s"Invalid WeekdayPeriod duration: $toString"))
    else
      Right(this)

  private[time] def dayName: String =
    WeekdaysNames(dayOffset)

  def dayOffset: Int =
    secondOfWeek / DaySeconds

  def secondOfDay: Int =
    secondOfWeek % DaySeconds

  override def toString = "WeekdayPeriod(" + dayName + " " +
    Timestamp.ofEpochSecond(secondOfDay).toTimeString + " " + duration.pretty + ")"
}

object WeekdayPeriod
{
  @TestOnly
  def apply(weekday: DayOfWeek, localTime: LocalTime, duration: FiniteDuration): WeekdayPeriod =
    new WeekdayPeriod(weekdayToSeconds(weekday) + localTime.toSecondOfDay, duration)
      .checked.orThrow
}

final case class DailyPeriod(secondOfDay: Int, duration: FiniteDuration)
extends AdmissionPeriod
{
  assertThat(secondOfDay >= 0 && secondOfDay <= DaySeconds)
  assert(duration <= DayDuration)

  def checked: Checked[DailyPeriod] =
    if (secondOfDay < 0 || secondOfDay >= DaySeconds)
      Left(Problem(s"Invalid daytime number: $toString"))
    else if (!duration.isPositive || duration > DayDuration)
      Left(Problem(s"Duration must be positive: $toString"))
    else
      Right(this)

  override def toString = "DailyPeriod(" + Timestamp.ofEpochSecond(secondOfDay).toTimeString +
      " " + duration.pretty + ")"
}
object DailyPeriod {
  val always = DailyPeriod(0, 24.h)

  @TestOnly
  def apply(localTime: LocalTime, duration: FiniteDuration): DailyPeriod =
    new DailyPeriod((localTime.toSecondOfDay), duration)
      .checked.orThrow
}

final case class MonthlyDatePeriod(secondOfMonth: Int, duration: FiniteDuration)
extends AdmissionPeriod
{
  def checked: Checked[this.type] =
    if (secondOfMonth < 0 || secondOfMonth >= 31*DaySeconds)
      Left(Problem(s"Invalid time in a month: $toString"))
    else if (!duration.isPositive)
      Left(Problem(s"Duration must be positive: $toString"))
    else
      Right(this)
}
object MonthlyDatePeriod {
  @TestOnly
  def apply(dayOfMonth: Int, timeOfDate: LocalTime, duration: FiniteDuration): MonthlyDatePeriod =
    apply((dayOfMonth - 1) * DaySeconds + timeOfDate.toSecondOfDay, duration)
      .checked.orThrow
}

/** Monthly admission at a specific last day of month.
 * @param lastSecondOfMonth for example, -3600 for the last day at 23:00. */
final case class MonthlyLastDatePeriod(lastSecondOfMonth: Int, duration: FiniteDuration)
extends AdmissionPeriod
{
  def checked: Checked[this.type] =
    if (lastSecondOfMonth <= -28*DaySeconds || lastSecondOfMonth >= 0)
      Left(Problem(s"Invalid reverse time in a month (must be negative): $toString"))
    else if (!duration.isPositive)
      Left(Problem(s"Duration must be positive: $toString"))
    else
      Right(this)
}
object MonthlyLastDatePeriod {
  @TestOnly
  def apply(lastDayOfMonth: Int, timeOfDate: LocalTime, duration: FiniteDuration)
  : MonthlyLastDatePeriod =
    apply((lastDayOfMonth + 1) * DaySeconds - (DaySeconds - timeOfDate.toSecondOfDay), duration)
      .checked.orThrow
}

/** Monthly admission at specific weekdays.
 * @param secondOfWeek may be > 7*24*3600. */
final case class MonthlyWeekdayPeriod(secondOfWeek: Int, duration: FiniteDuration)
extends AdmissionPeriod
{
  def checked: Checked[this.type] =
    if (secondOfWeek < 0 || secondOfWeek >= 4*WeekSeconds)
      Left(Problem(s"Invalid time in a month: $toString"))
    else if (!duration.isPositive)
      Left(Problem(s"Duration must be positive: $toString"))
    else
      Right(this)
}
object MonthlyWeekdayPeriod {
  @TestOnly
  def apply(number: Int, weekday: DayOfWeek, localTime: LocalTime, duration: FiniteDuration)
  : MonthlyWeekdayPeriod =
    apply(
      (number - 1) * WeekSeconds + weekdayToSeconds(weekday) + localTime.toSecondOfDay,
      duration
    ).checked.orThrow
}

object AdmissionPeriod
{
  private[time] val DaySeconds = 24 * 3600
  private[time] val DayDuration = Duration(1, DAYS)
  private[time] val WeekSeconds = 7 * DaySeconds
  private[time] val WeekDuration = Duration(7, DAYS)
  private[time] val WeekdaysNames =
    Vector("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

  private[time] def weekdayToSeconds(dayOfWeek: DayOfWeek) =
    (dayOfWeek.getValue - 1) * DaySeconds

  implicit val jsonCodec = TypedJsonCodec[AdmissionPeriod](
    Subtype(AlwaysPeriod),
    Subtype(deriveCodec[WeekdayPeriod].checked(_.checked)),
    Subtype(deriveCodec[DailyPeriod].checked(_.checked)),
    Subtype(deriveCodec[MonthlyDatePeriod].checked(_.checked)),
    Subtype(deriveCodec[MonthlyLastDatePeriod].checked(_.checked)),
    Subtype(deriveCodec[MonthlyWeekdayPeriod].checked(_.checked)))

  intelliJuseImport(FiniteDurationJsonEncoder)
}
