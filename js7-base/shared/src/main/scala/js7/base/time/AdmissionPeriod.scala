package js7.base.time

import io.circe.generic.semiauto.deriveCodec
import java.time.{DayOfWeek, LocalDateTime, LocalTime, ZoneOffset}
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.{Checked, Problem}
import js7.base.time.AdmissionPeriod.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.ordinalToString
import js7.base.utils.ScalaUtils.syntax.*
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.*

/** Periodical admission time expressed in local time. */
sealed trait AdmissionPeriod:
  def pretty: String

  override def toString = s"${getClass.simpleScalaName}($pretty)"


case object AlwaysPeriod extends AdmissionPeriod:
  def pretty = "always"

  override def toString = "Always"


final case class DailyPeriod(secondOfDay: Int, duration: FiniteDuration)
extends AdmissionPeriod:
  assertThat(secondOfDay >= 0 && secondOfDay <= DaySeconds)
  assert(duration <= DayDuration)

  def checked: Checked[DailyPeriod] =
    if secondOfDay < 0 || secondOfDay >= DaySeconds then
      Left(Problem(s"Invalid daytime number: $toString"))
    else if !duration.isPositive || duration > DayDuration then
      Left(Problem(s"Duration must be positive: $toString"))
    else
      Right(this)

  def pretty: String =
    "daily at " + secondsOfDayToString(secondOfDay) + ", " + duration.pretty

object DailyPeriod:
  val always: DailyPeriod = DailyPeriod(0, 24.h)

  @TestOnly
  def apply(localTime: LocalTime, duration: FiniteDuration): DailyPeriod =
    new DailyPeriod((localTime.toSecondOfDay), duration)
      .checked.orThrow


/** Weekly admission time. */
final case class WeekdayPeriod(secondOfWeek: Int, duration: FiniteDuration)
extends AdmissionPeriod:
  def checked: Checked[this.type] =
    if secondOfWeek < 0 || secondOfWeek >= WeekSeconds then
      Left(Problem(s"Invalid weekday time number: $toString"))
    else if !duration.isPositive || duration > WeekDuration then
      Left(Problem(s"Invalid WeekdayPeriod duration: $toString"))
    else
      Right(this)

  private[time] def dayName: String =
    WeekdaysNames.checked(dayOfWeek) getOrElse "?"

  def dayOfWeek: Int =
    secondOfWeek / DaySeconds

  def secondOfDay: Int =
    secondOfWeek % DaySeconds

  def pretty: String =
    "weekly at " + dayName + " " + secondsOfDayToString(secondOfDay) + ", " + duration.pretty

object WeekdayPeriod:
  @TestOnly
  def apply(weekday: DayOfWeek, localTime: LocalTime, duration: FiniteDuration): WeekdayPeriod =
    new WeekdayPeriod(weekdayToSeconds(weekday) + localTime.toSecondOfDay, duration)
      .checked.orThrow


final case class MonthlyDatePeriod(secondOfMonth: Int, duration: FiniteDuration)
extends AdmissionPeriod:
  def checked: Checked[this.type] =
    if secondOfMonth < 0 || secondOfMonth >= 31*DaySeconds then
      Left(Problem(s"Invalid time in a month: $toString"))
    else if !duration.isPositive then
      Left(Problem(s"Duration must be positive: $toString"))
    else
      Right(this)

  private def dayOfMonth = secondOfMonth / DaySeconds + 1

  def secondOfDay: Int =
    secondOfMonth % DaySeconds

  def pretty: String =
    ordinalToString(dayOfMonth) + " of month, " +
      secondsOfDayToString(secondOfDay) + ", " +
      duration.pretty

object MonthlyDatePeriod:
  @TestOnly
  def apply(dayOfMonth: Int, timeOfDate: LocalTime, duration: FiniteDuration): MonthlyDatePeriod =
    apply((dayOfMonth - 1) * DaySeconds + timeOfDate.toSecondOfDay, duration)
      .checked.orThrow


/** Monthly admission at a specific last day of month.
 * @param lastSecondOfMonth for example, -3600 for the last day at 23:00. */
final case class MonthlyLastDatePeriod(lastSecondOfMonth: Int, duration: FiniteDuration)
extends AdmissionPeriod:
  def checked: Checked[this.type] =
    if lastSecondOfMonth <= -28*DaySeconds || lastSecondOfMonth >= 0 then
      Left(Problem(s"Invalid reverse time in a month (must be negative): $toString"))
    else if !duration.isPositive then
      Left(Problem(s"Duration must be positive: $toString"))
    else
      Right(this)

  private def lastDayOfMonth = -lastSecondOfMonth / DaySeconds + 1

  def secondOfDay: Int =
    -lastSecondOfMonth % DaySeconds

  def pretty: String =
    (lastDayOfMonth match
      case 1 => ""
      case _ => ordinalToString(lastDayOfMonth) + " to "
    ) + "last day of month, " +
      secondsOfDayToString(secondOfDay) + ", " +
      duration.pretty

object MonthlyLastDatePeriod:
  @TestOnly
  def apply(lastDayOfMonth: Int, timeOfDate: LocalTime, duration: FiniteDuration)
  : MonthlyLastDatePeriod =
    apply((lastDayOfMonth + 1) * DaySeconds - (DaySeconds - timeOfDate.toSecondOfDay), duration)
      .checked.orThrow


/** Monthly admission at specific weekdays.
 * @param secondOfWeeks may be > 7*24*3600. */
final case class MonthlyWeekdayPeriod(secondOfWeeks: Int, duration: FiniteDuration)
extends AdmissionPeriod:
  def checked: Checked[this.type] =
    if secondOfWeeks < 0 || secondOfWeeks >= 4*WeekSeconds then
      Left(Problem(s"Invalid time in a month: $secondOfWeeks $toString"))
    else if !duration.isPositive then
      Left(Problem(s"Duration must be positive: $toString"))
    else
      Right(this)

  def shiftWeeks: Int =
    secondOfWeeks / WeekSeconds

  def dayOfWeek: Int =
    secondOfWeeks / DaySeconds % 7

  def secondOfDay: Int =
    secondOfWeeks % DaySeconds

  def pretty: String =
    ordinalToString(shiftWeeks + 1) + " " +
      WeekdaysNames.checked(dayOfWeek).getOrElse("?") + " " +
      secondsOfDayToString(secondOfDay) + ", " +
      duration.pretty

object MonthlyWeekdayPeriod:
  @TestOnly
  def apply(number: Int, weekday: DayOfWeek, localTime: LocalTime, duration: FiniteDuration)
  : MonthlyWeekdayPeriod =
    apply(
      (number - 1) * WeekSeconds + weekdayToSeconds(weekday) + localTime.toSecondOfDay,
      duration
    ).checked.orThrow


/** Monthly admission at specific last weekdays.
 * @param secondOfWeeks may be > 7*24*3600. */
final case class MonthlyLastWeekdayPeriod(
  secondOfWeeks: Int,
  duration: FiniteDuration)
extends AdmissionPeriod:
  def checked: Checked[this.type] =
    if secondOfWeeks <= -4*WeekSeconds || secondOfWeeks >= 0 then
      Left(Problem(s"Invalid time in a month: $toString"))
    else if !duration.isPositive then
      Left(Problem(s"Duration must be positive: $toString"))
    else
      Right(this)

  /** 0: last week. */
  private[time] def shiftWeeks = (secondOfWeeks + 1) / WeekSeconds

  def dayOfWeek: Int =
    ((secondOfWeeks + 5*WeekSeconds) / DaySeconds) % 7

  def secondOfDay: Int =
    (secondOfWeeks + 5*WeekSeconds) % DaySeconds

  def secondOfWeek: Int = (secondOfWeeks + 5*WeekSeconds) % WeekSeconds

  def pretty: String =
    (shiftWeeks match
      case 0 => "last "
      case n => ordinalToString(-n+1) + " last "
    ) +
      WeekdaysNames.checked(dayOfWeek).getOrElse("?") + " " +
      secondsOfDayToString(secondOfDay) + ", " +
      duration.pretty

object MonthlyLastWeekdayPeriod:
  @TestOnly
  def apply(
    week: Int,
    weekday: DayOfWeek,
    localTime: LocalTime,
    duration: FiniteDuration)
  : MonthlyLastWeekdayPeriod =
    apply(
      week * WeekSeconds + weekdayToSeconds(weekday) + localTime.toSecondOfDay,
      duration
    ).checked.orThrow


final case class SpecificDatePeriod(secondsSinceLocalEpoch: Long, duration: FiniteDuration)
extends AdmissionPeriod:
  override def pretty: String =
    val ts = Timestamp.ofEpochSecond(secondsSinceLocalEpoch).toString.stripSuffix("Z")
    s"$ts, ${duration.pretty}"

object SpecificDatePeriod:
  @TestOnly
  def apply(localDateTime: LocalDateTime, duration: FiniteDuration): SpecificDatePeriod =
    new SpecificDatePeriod(
      localDateTime.toEpochSecond(ZoneOffset.ofTotalSeconds(0)),
      duration)


object AdmissionPeriod:
  private[time] val DaySeconds = 24 * 3600
  private[time] val DayDuration = Duration(1, DAYS)
  private[time] val WeekSeconds = 7 * DaySeconds
  private[time] val WeekDuration = Duration(7, DAYS)
  private[time] val WeekdaysNames =
    Vector("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

  private[time] def weekdayToSeconds(dayOfWeek: DayOfWeek) =
    (dayOfWeek.getValue - 1) * DaySeconds

  private[time] def secondsOfDayToString(secondOfDay: Int) =
    val hh = secondOfDay / 3600
    val mm = secondOfDay / 60 % 60
    val ss = secondOfDay % 60
    f"$hh%02d:$mm%02d" + ((ss != 0) ?? f":$ss%02d")

  implicit val jsonCodec: TypedJsonCodec[AdmissionPeriod] = TypedJsonCodec(
    Subtype(AlwaysPeriod),
    Subtype(deriveCodec[WeekdayPeriod].checked(_.checked)),
    Subtype(deriveCodec[DailyPeriod].checked(_.checked)),
    Subtype(deriveCodec[MonthlyDatePeriod].checked(_.checked)),
    Subtype(deriveCodec[MonthlyLastDatePeriod].checked(_.checked)),
    Subtype(deriveCodec[MonthlyWeekdayPeriod].checked(_.checked)),
    Subtype(deriveCodec[MonthlyLastWeekdayPeriod].checked(_.checked)),
    Subtype(deriveCodec[SpecificDatePeriod]))

  intelliJuseImport(FiniteDurationJsonEncoder)
