package js7.base.time

import cats.syntax.show.*
import io.circe.generic.semiauto.deriveCodec
import java.time.{DayOfWeek, LocalDateTime, LocalTime, ZoneOffset}
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.{Checked, Problem}
import js7.base.time.AdmissionPeriod.*
import js7.base.time.Problems.PeriodCrossesProductionDayBoundaryProblem
import js7.base.time.ScalaTime.*
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.ScalaUtils.ordinalToString
import js7.base.utils.ScalaUtils.syntax.*
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.*

/** Periodical admission time expressed in local time. */
sealed trait AdmissionPeriod:
  def pretty: String

  protected def prettyRaw: String =
    pretty

  protected def checked: Checked[this.type]

  final def check(dateOffset: FiniteDuration): Seq[PeriodCrossesProductionDayBoundaryProblem] =
    checked.fold(_.toOption.toList, _ => checkAgainstDateOffset(dateOffset))

  protected def checkAgainstDateOffset(dateOffset: FiniteDuration)
  : List[PeriodCrossesProductionDayBoundaryProblem]

  override def toString = s"${getClass.simpleScalaName}($prettyRaw)"


final case class DailyPeriod private(secondOfDay: Int, duration: FiniteDuration)
extends AdmissionPeriod:
  def checked: Checked[this.type] =
    if secondOfDay < 0 || secondOfDay >= DaySeconds then
      Left(Problem(s"Invalid daytime number: $toString"))
    else if !duration.isPositive then
      Left(Problem(s"Duration must be positive: $toString"))
    else
      Right(this)

  protected def checkAgainstDateOffset(dateOffset: FiniteDuration)
  : List[PeriodCrossesProductionDayBoundaryProblem] =
    val begin = secondOfDay.s
    if begin < dateOffset && begin + duration > dateOffset
      || begin + duration > OneDay && begin + duration - OneDay > dateOffset
    then
      PeriodCrossesProductionDayBoundaryProblem(this, dateOffset) :: Nil
    else
      Nil

  def pretty: String =
    "daily at " + prettyRaw

  override protected def prettyRaw: String =
    secondsOfDayToString(secondOfDay) + ", " + duration.show

object DailyPeriod:
  val always: DailyPeriod = new DailyPeriod(0, 24.h).checked.orThrow

  @TestOnly
  def apply(secondOfDay: Int, duration: FiniteDuration): DailyPeriod =
    new DailyPeriod(secondOfDay, duration)
      .checked.orThrow

  @TestOnly
  def apply(localTime: LocalTime, duration: FiniteDuration): DailyPeriod =
    new DailyPeriod(localTime.toSecondOfDay, duration)
      .checked.orThrow

  def checked(localTime: LocalTime, duration: FiniteDuration): Checked[DailyPeriod] =
    new DailyPeriod((localTime.toSecondOfDay), duration)
      .checked


/** Weekly admission time. */
final case class WeekdayPeriod private(secondOfWeek: Int, duration: FiniteDuration)
extends AdmissionPeriod:
  def checked: Checked[this.type] =
    if secondOfWeek < 0 || secondOfWeek >= WeekSeconds then
      Left(Problem(s"Invalid weekday time number: $toString"))
    else if !duration.isPositive || duration > WeekDuration then
      Left(Problem(s"Invalid WeekdayPeriod duration: $toString"))
    else
      Right(this)

  protected def checkAgainstDateOffset(dateOffset: FiniteDuration)
  : List[PeriodCrossesProductionDayBoundaryProblem] =
    val begin = secondOfWeek.s
    if begin < dateOffset && begin + duration > dateOffset
      || begin + duration > OneWeek && begin + duration - OneWeek > dateOffset
    then
      PeriodCrossesProductionDayBoundaryProblem(this, dateOffset) :: Nil
    else
      Nil

  private[time] def dayName: String =
    WeekdaysNames.getOrElse(dayOfWeek)("?")

  def dayOfWeek: Int =
    secondOfWeek / DaySeconds

  def secondOfDay: Int =
    secondOfWeek % DaySeconds

  def pretty: String =
    "weekly at " + prettyRaw

  override protected def prettyRaw: String =
    dayName + " " + secondsOfDayToString(secondOfDay) + ", " + duration.pretty

object WeekdayPeriod:
  @TestOnly
  def apply(secondOfWeek: Int, duration: FiniteDuration): WeekdayPeriod =
    new WeekdayPeriod(secondOfWeek, duration)
      .checked.orThrow

  @TestOnly
  def apply(weekday: DayOfWeek, localTime: LocalTime, duration: FiniteDuration): WeekdayPeriod =
    new WeekdayPeriod(weekdayToSeconds(weekday) + localTime.toSecondOfDay, duration)
      .checked.orThrow


final case class MonthlyDatePeriod private(secondOfMonth: Int, duration: FiniteDuration)
extends AdmissionPeriod:
  import MonthlyDatePeriod.*

  def checked: Checked[this.type] =
    if secondOfMonth < 0 || secondOfMonth >= 31*DaySeconds then
      Left(Problem(s"Invalid time in a month: $toString"))
    else if !duration.isPositive then
      Left(Problem(s"Duration must be positive: $toString"))
    else
      Right(this)

  protected def checkAgainstDateOffset(dateOffset: FiniteDuration)
  : List[PeriodCrossesProductionDayBoundaryProblem] =
    val begin = secondOfMonth.s
    if begin < dateOffset && begin + duration > dateOffset
      || begin + duration > MonthMax && begin + duration - MonthMax > dateOffset
    then
      PeriodCrossesProductionDayBoundaryProblem(this, dateOffset) :: Nil
    else
      Nil

  private def dayOfMonth = secondOfMonth / DaySeconds + 1

  def secondOfDay: Int =
    secondOfMonth % DaySeconds

  def pretty: String =
    ordinalToString(dayOfMonth) + " of month, " +
      secondsOfDayToString(secondOfDay) + ", " +
      duration.pretty

object MonthlyDatePeriod:
  private val MonthMax = 31 * OneDay

  @TestOnly
  def apply(secondOfMonth: Int, duration: FiniteDuration): MonthlyDatePeriod =
    new MonthlyDatePeriod(secondOfMonth, duration)
      .checked.orThrow

  @TestOnly
  def apply(dayOfMonth: Int, timeOfDate: LocalTime, duration: FiniteDuration): MonthlyDatePeriod =
    apply((dayOfMonth - 1) * DaySeconds + timeOfDate.toSecondOfDay, duration)
      .checked.orThrow


/** Monthly admission at a specific last day of month.
 * @param lastSecondOfMonth for example, -3600 for the last day at 23:00. */
final case class MonthlyLastDatePeriod private(lastSecondOfMonth: Int, duration: FiniteDuration)
extends AdmissionPeriod:
  import MonthlyLastDatePeriod.*

  def checked: Checked[this.type] =
    if lastSecondOfMonth < LowestSeconds || lastSecondOfMonth >= 0 then
      Left(Problem(s"Invalid reverse time in a month (must be negative): $toString"))
    else if !duration.isPositive then
      Left(Problem(s"Duration must be positive: $toString"))
    else
      Right(this)

  protected def checkAgainstDateOffset(dateOffset: FiniteDuration)
  : List[PeriodCrossesProductionDayBoundaryProblem] =
    val begin = lastSecondOfMonth.s
    if begin < dateOffset && begin + duration > dateOffset
      || begin + duration > ZeroDuration && begin + duration > dateOffset
    then
      PeriodCrossesProductionDayBoundaryProblem(this, dateOffset) :: Nil
    else
      Nil
    //val begin = lastSecondOfMonth.s - OneDay - Lowest
    //if begin < dateOffset && begin + duration > dateOffset
    //  || begin + duration > ZeroDuration && begin + duration > dateOffset
    //then
    //  PeriodCrossesProductionDayBoundaryProblem(this, dateOffset) :: Nil
    //else
    //  Nil

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
  private[time] val LowestSeconds = -28 * DaySeconds
  private[time] val Lowest = LowestSeconds.seconds.toCoarsest
  @TestOnly
  def apply(lastSecondOfMonth: Int, duration: FiniteDuration): MonthlyLastDatePeriod =
    new MonthlyLastDatePeriod(lastSecondOfMonth, duration)
      .checked.orThrow

  @TestOnly
  def apply(lastDayOfMonth: Int, timeOfDate: LocalTime, duration: FiniteDuration)
  : MonthlyLastDatePeriod =
    apply((lastDayOfMonth + 1) * DaySeconds - (DaySeconds - timeOfDate.toSecondOfDay), duration)
      .checked.orThrow


/** Monthly admission at specific weekdays.
 * @param secondOfWeeks may be > 7 * 24 * 3600. */
final case class MonthlyWeekdayPeriod private(secondOfWeeks: Int, duration: FiniteDuration)
extends AdmissionPeriod:
  def checked: Checked[this.type] =
    if secondOfWeeks < 0 || secondOfWeeks >= 4*WeekSeconds then
      Left(Problem(s"Invalid time in a month: $secondOfWeeks $toString"))
    else if !duration.isPositive then
      Left(Problem(s"Duration must be positive: $toString"))
    else
      Right(this)

  protected def checkAgainstDateOffset(dateOffset: FiniteDuration)
  : List[PeriodCrossesProductionDayBoundaryProblem] =
    Nil // TODO

  def shiftWeeks: Int =
    secondOfWeeks / WeekSeconds

  def dayOfWeek: Int =
    secondOfWeeks / DaySeconds % 7

  def secondOfDay: Int =
    secondOfWeeks % DaySeconds

  def pretty: String =
    ordinalToString(shiftWeeks + 1) + " " +
      WeekdaysNames.getOrElse(dayOfWeek)("?") + " " +
      secondsOfDayToString(secondOfDay) + ", " +
      duration.pretty

object MonthlyWeekdayPeriod:
  @TestOnly
  def apply(secondOfWeeks: Int, duration: FiniteDuration): MonthlyWeekdayPeriod =
    new MonthlyWeekdayPeriod(secondOfWeeks, duration)
      .checked.orThrow

  @TestOnly
  def apply(number: Int, weekday: DayOfWeek, localTime: LocalTime, duration: FiniteDuration)
  : MonthlyWeekdayPeriod =
    apply(
      (number - 1) * WeekSeconds + weekdayToSeconds(weekday) + localTime.toSecondOfDay,
      duration
    ).checked.orThrow


/** Monthly admission at specific last weekdays.
  * @param secondOfWeeks may be > 7 * 24 * 3600. */
final case class MonthlyLastWeekdayPeriod private(secondOfWeeks: Int, duration: FiniteDuration)
extends AdmissionPeriod:
  def checked: Checked[this.type] =
    if secondOfWeeks <= -4*WeekSeconds || secondOfWeeks >= 0 then
      Left(Problem(s"Invalid time in a month: $toString"))
    else if !duration.isPositive then
      Left(Problem(s"Duration must be positive: $toString"))
    else
      Right(this)

  protected def checkAgainstDateOffset(dateOffset: FiniteDuration)
  : List[PeriodCrossesProductionDayBoundaryProblem] =
    Nil // No check possible?

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
      WeekdaysNames.getOrElse(dayOfWeek)("?") + " " +
      secondsOfDayToString(secondOfDay) + ", " +
      duration.pretty

object MonthlyLastWeekdayPeriod:
  @TestOnly
  def apply(secondOfWeeks: Int, duration: FiniteDuration): MonthlyLastWeekdayPeriod =
    new MonthlyLastWeekdayPeriod(secondOfWeeks, duration)
      .checked.orThrow

  /** @param week -1...-5 */
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


final case class SpecificDatePeriod private(secondsSinceLocalEpoch: Long, duration: FiniteDuration)
extends AdmissionPeriod:
  // TODO duration may be cross a daylight-saving time boundary, which may not be respected
  //  by MonthRestriction#clipLocalInterval.
  def checked: Checked[this.type] =
    if !duration.isPositive then
      Left(Problem(s"Duration must be positive: $toString"))
    else
      Right(this)

  protected def checkAgainstDateOffset(dateOffset: FiniteDuration)
  : List[PeriodCrossesProductionDayBoundaryProblem] =
    Nil // No check possible?

  override def pretty: String =
    val ts = Timestamp.ofEpochSecond(secondsSinceLocalEpoch).toString.stripSuffix("Z")
    s"$ts, ${duration.pretty}"

object SpecificDatePeriod:
  @TestOnly
  def apply(secondsSinceLocalEpoch: Long, duration: FiniteDuration): SpecificDatePeriod =
    new SpecificDatePeriod(secondsSinceLocalEpoch, duration).checked.orThrow

  @TestOnly
  def apply(localDateTime: LocalDateTime, duration: FiniteDuration): SpecificDatePeriod =
    apply(
      localDateTime.toEpochSecond(ZoneOffset.ofTotalSeconds(0)),
      duration)

  def checked(secondsSinceLocalEpoch: Long, duration: FiniteDuration): Checked[SpecificDatePeriod] =
    SpecificDatePeriod(secondsSinceLocalEpoch, duration).checked


object AdmissionPeriod:
  private[time] inline val DaySeconds = 24 * 3600
  private[time] val OneDay = 1.day
  private[time] val DayDuration = Duration(1, DAYS)
  private[time] inline val WeekSeconds = 7 * DaySeconds
  private[time] val OneWeek = 7.days
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
    //Subtype(AlwaysPeriod),
    Subtype(deriveCodec[WeekdayPeriod].checked(_.checked)),
    Subtype(deriveCodec[DailyPeriod].checked(_.checked)),
    Subtype(deriveCodec[MonthlyDatePeriod].checked(_.checked)),
    Subtype(deriveCodec[MonthlyLastDatePeriod].checked(_.checked)),
    Subtype(deriveCodec[MonthlyWeekdayPeriod].checked(_.checked)),
    Subtype(deriveCodec[MonthlyLastWeekdayPeriod].checked(_.checked)),
    Subtype(deriveCodec[SpecificDatePeriod].checked(_.checked)))

  intelliJuseImport(FiniteDurationJsonEncoder)
