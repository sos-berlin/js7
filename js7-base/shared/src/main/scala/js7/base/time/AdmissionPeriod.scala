package js7.base.time

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import java.time.{DayOfWeek, LocalTime}
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.time.WeekdayPeriod._
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

  override def toString =
    f"WeekdayPeriod($dayName ${secondOfDay / 3600}%02d:${secondOfDay / 60 % 60}%02d:${secondOfDay % 60}%02d)"
}

object WeekdayPeriod
{
  private val DaySeconds = 24 * 3600
  private val WeekSeconds = 7 * DaySeconds
  private val WeekDuration = Duration(7, DAYS)
  private val WeekdaysNames = Vector("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
    "Saturday", "Sunday")

  // Only for JVM !!!
  def apply(weekday: DayOfWeek, localTime: LocalTime, duration: FiniteDuration): WeekdayPeriod =
    checked(((weekday.getValue - 1) * DaySeconds + localTime.toSecondOfDay), duration)
      .orThrow

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

object AdmissionPeriod
{
  implicit val jsonCodec = TypedJsonCodec[AdmissionPeriod](
    Subtype[WeekdayPeriod])
}
