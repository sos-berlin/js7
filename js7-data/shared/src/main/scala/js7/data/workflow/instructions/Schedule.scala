package js7.data.workflow.instructions

import io.circe.generic.semiauto.{deriveCodec, deriveEncoder}
import io.circe.{Codec, Decoder, Encoder}
import js7.base.circeutils.CirceUtils.*
import js7.base.circeutils.ScalaJsonCodecs.*
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.{Checked, Problem}
import js7.base.time.AdmissionTimeScheme
import js7.base.time.ScalaTime.RichFiniteDuration
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.workflow.instructions.Schedule.Scheme
import scala.collection.View
import scala.concurrent.duration.*

final case class Schedule(schemes: Seq[Scheme])


object Schedule:

  val never: Schedule = Schedule(Nil)

  def continuous(pause: FiniteDuration, limit: Option[Int] = None): Schedule =
    Schedule(Seq(Scheme(
      AdmissionTimeScheme.always,
      Continuous(pause, limit = limit))))

  def ticking(interval: FiniteDuration): Schedule =
    Schedule(Seq(Scheme(
      AdmissionTimeScheme.always,
      Ticking(interval))))


  final case class Scheme(admissionTimeScheme: AdmissionTimeScheme, repeat: Repeat)

  object Scheme:
    given Codec.AsObject[Scheme] = deriveCodec[Scheme]


  sealed trait Repeat

  object Repeat:
    given TypedJsonCodec[Repeat] = TypedJsonCodec(
      Subtype[Periodic],
      Subtype[Ticking],
      Subtype[Continuous])


  final case class Periodic(period: FiniteDuration, offsets: Seq[FiniteDuration])
  extends Repeat:
    override def toString: String = "period=" + period +
      " offsets=" + offsets.map(_.pretty).mkString("(", ", ", ")")

  object Periodic:
    def checked(period: FiniteDuration, offsets: Seq[FiniteDuration]): Checked[Periodic] =
      if period.isPositive
        && offsets.exists(o => !o.isNegative & o < period)
        && offsets.areUnique then
        Right(Periodic(period, offsets))
      else
        Left(Problem.pure("Invalid Periodic arguments"))

    given Encoder.AsObject[Periodic] = deriveEncoder
    given Decoder[Periodic] =
      c => for
        period <- c.get[FiniteDuration]("period")
        offsets <- c.get[Vector[FiniteDuration]]("offsets")
        _ <- checked(period, offsets).toDecoderResult(c.history)
      yield Periodic(period, offsets.sorted)


  /** Ticking with fixed interval such that start times can be calculated ahead.
    */
  final case class Ticking(interval: FiniteDuration)
  extends Repeat:
    override def toString = s"Ticking(${interval.pretty})"

  object Ticking:
    def checked(interval: FiniteDuration): Checked[Ticking] =
      if !interval.isPositive then
        Left(Problem.pure("Invalid Ticking arguments"))
      else
        Right(Ticking(interval))

    given Encoder.AsObject[Ticking] = deriveEncoder
    given Decoder[Ticking] = c =>
      for
        interval <- c.get[FiniteDuration]("interval")
        ticking <- checked(interval).toDecoderResult(c.history)
      yield
        ticking


  /** Continuous with a pause between each cycle.
    */
  final case class Continuous(
    pause: FiniteDuration,
    limit: Option[Int] = None)
  extends Repeat:
    override def toString: String =
      View(
        Some("pause=" + pause.pretty),
        limit.map("limit=" + _)
      ).mkString("Continuous(", " ", ")")

  object Continuous:
    def checked(
      pause: FiniteDuration = Duration.Zero,
      limit: Option[Int] = None)
    : Checked[Continuous] =
      if pause.isNegative || !limit.forall(_ >= 0) then
        Left(Problem.pure("Invalid Continuous arguments"))
      else if !pause.isPositive & limit.isEmpty then
        Left(Problem.pure("Continuous: limit or pause must be set"))
      else
        Right(Continuous(pause, limit))

    given Encoder.AsObject[Continuous] = deriveEncoder
    given Decoder[Continuous] = c =>
      for
        pause <- c.get[FiniteDuration]("pause")
        limit <- c.get[Option[Int]]("limit")
        continuous <- checked(pause, limit).toDecoderResult(c.history)
      yield
        continuous


  given Codec.AsObject[Schedule] = deriveCodec[Schedule]

  intelliJuseImport(FiniteDurationJsonDecoder)
