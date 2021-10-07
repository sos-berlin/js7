package js7.base.time

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import java.time.{DayOfWeek, LocalTime}
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.{Checked, Problem}
import js7.base.time.AdmissionPeriod._
import js7.base.time.ScalaTime._
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax._
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

  private[time] def dayName: String =
    WeekdaysNames(dayNumber - 1)

  private[time] def dayNumber: Int =
    1 + secondOfWeek / DaySeconds

  private def secondOfDay: Int =
    secondOfWeek % DaySeconds

  override def toString = "WeekdayPeriod(" + dayName + " " +
    Timestamp.ofEpochSecond(secondOfDay).toTimeString + " " + duration.pretty + ")"
}

object WeekdayPeriod
{
  // Only for JVM !!!
  def apply(weekday: DayOfWeek, localTime: LocalTime, duration: FiniteDuration): WeekdayPeriod =
    checked((weekdayToSeconds(weekday) + localTime.toSecondOfDay), duration)
      .orThrow

  private def weekdayToSeconds(dayOfWeek: DayOfWeek) =
    (dayOfWeek.getValue - 1) * DaySeconds

  def checked(secondOfWeek: Int, duration: FiniteDuration): Checked[WeekdayPeriod] =
    if (secondOfWeek < 0 || secondOfWeek >= WeekSeconds)
      Left(Problem(s"Invalid weekday time number: $secondOfWeek"))
    else if (!duration.isPositive || duration > WeekDuration)
      Left(Problem(s"Invalid WeekdayPeriod duration: ${duration.pretty}"))
    else
      Right(WeekdayPeriod(secondOfWeek, duration))

  implicit val jsonEncoder: Encoder.AsObject[WeekdayPeriod] =
    o => JsonObject(
      "secondOfWeek" -> o.secondOfWeek.asJson,
      "duration" -> o.duration.toSeconds.asJson)

  implicit val jsonDecoder: Decoder[WeekdayPeriod] =
    c => for {
      secondOfWeek <- c.get[Int]("secondOfWeek")
      duration <- c.get[FiniteDuration]("duration")
      weekdayTime <- checked(secondOfWeek, duration).toDecoderResult(c.history)
    } yield weekdayTime
}

/** Weekly admission time. */
final case class DailyPeriod(secondOfDay: Int, duration: FiniteDuration)
extends AdmissionPeriod
{
  assertThat(secondOfDay >= 0 && secondOfDay <= DaySeconds)
  assert(duration <= DayDuration)

  override def toString = "DailyPeriod(" + Timestamp.ofEpochSecond(secondOfDay).toTimeString +
      " " + duration.pretty + ")"
}
object DailyPeriod {
  val always = DailyPeriod(0, 24.h)

  // Only for JVM !!!
  def apply(localTime: LocalTime, duration: FiniteDuration): DailyPeriod =
    checked((localTime.toSecondOfDay), duration)
      .orThrow

  def checked(secondOfDay: Int, duration: FiniteDuration): Checked[DailyPeriod] =
    if (secondOfDay < 0 || secondOfDay >= DaySeconds)
      Left(Problem(s"Invalid daytime number: $secondOfDay"))
    else if (!duration.isPositive || duration > DayDuration)
      Left(Problem(s"Invalid DailyPeriod duration: ${duration.pretty}"))
    else
      Right(DailyPeriod(secondOfDay, duration))

  implicit val jsonEncoder: Encoder.AsObject[DailyPeriod] =
    o => JsonObject(
      "secondOfDay" -> o.secondOfDay.asJson,
      "duration" -> o.duration.toSeconds.asJson)

  implicit val jsonDecoder: Decoder[DailyPeriod] =
    c => for {
      secondOfDay <- c.get[Int]("secondOfDay")
      duration <- c.get[FiniteDuration]("duration")
      weekdayTime <- checked(secondOfDay, duration).toDecoderResult(c.history)
    } yield weekdayTime
}

object AdmissionPeriod
{
  private[time] val DaySeconds = 24 * 3600
  private[time] val DayDuration = Duration(1, DAYS)
  private[time] val WeekSeconds = 7 * DaySeconds
  private[time] val WeekDuration = Duration(7, DAYS)
  private[time] val WeekdaysNames = Vector("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
    "Saturday", "Sunday")

  implicit val jsonCodec = TypedJsonCodec[AdmissionPeriod](
    Subtype(AlwaysPeriod),
    Subtype[WeekdayPeriod],
    Subtype(deriveCodec[DailyPeriod]))
}
