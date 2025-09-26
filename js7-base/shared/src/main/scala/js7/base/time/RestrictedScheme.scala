package js7.base.time

import io.circe
import io.circe.generic.semiauto.{deriveCodec, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, Encoder, JsonObject}
import java.time.ZoneOffset.UTC
import java.time.{LocalDateTime, Duration as JDuration}
import js7.base.circeutils.CirceUtils.toDecoderResult
import js7.base.circeutils.ScalaJsonCodecs.BitSetCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.time.SchemeRestriction.MonthRestriction.MonthNames
import js7.base.time.SchemeRestriction.Unrestricted
import js7.base.utils.ScalaUtils.syntax.*
import scala.collection.immutable.BitSet

final case class RestrictedScheme(
  periods: Seq[AdmissionPeriod],
  restriction: SchemeRestriction = Unrestricted)

object RestrictedScheme:
  given Encoder.AsObject[RestrictedScheme] =
    o => JsonObject(
      "periods" -> o.periods.asJson,
      "restriction" -> ((o.restriction ne Unrestricted) ? o.restriction).asJson)

  given Decoder[RestrictedScheme] =
    c => for
      periods <- c.get[Vector[AdmissionPeriod]]("periods")
      restriction <- c.getOrElse[SchemeRestriction]("restriction")(Unrestricted)
    yield
      RestrictedScheme(periods, restriction)


sealed trait SchemeRestriction:
  private[time] def isUnrestricted(local: LocalDateTime, dateOffset: JDuration): Boolean

  private[time] def skipRestriction(local: LocalDateTime, dateOffset: JDuration): LocalDateTime

  private[time] def clipLocalInterval(localInterval: LocalInterval, dateOffset: JDuration)
  : Option[LocalInterval]


object SchemeRestriction:
  private val AllMonths = (1 to 12).to(BitSet)

  def months(months: Set[Int]): Checked[SchemeRestriction] =
    if months == AllMonths then
      Right(Unrestricted)
    else
      MonthRestriction.checked(months)


  given TypedJsonCodec[SchemeRestriction] = TypedJsonCodec[SchemeRestriction](
    Subtype(Unrestricted),
    Subtype[MonthRestriction])


  case object Unrestricted extends SchemeRestriction:
    private[time] def isUnrestricted(local: LocalDateTime, dateOffset: JDuration): Boolean =
      true

    private[time] def skipRestriction(local: LocalDateTime, dateOffset: JDuration): LocalDateTime =
      local

    private[time] def clipLocalInterval(localInterval: LocalInterval, dateOffset: JDuration)
    : Option[LocalInterval] =
      Some(localInterval)


  // MonthRestriction //
  /** Admission at specific months.
    *
    * Only the month of localInterval.start is checked
    * @param months 1..12. */
  private[time] final case class MonthRestriction private(months: BitSet) extends SchemeRestriction:
    def checked: Checked[this.type] =
      if !months.forall(m => m >= 1 && m <= 12) then
        Left(Problem.pure("Month must be a number between 1 and 12"))
      else if months.isEmpty || months == AllMonths then
        Left(Problem.pure("MonthRestriction must contain between 1 and 11 months"))
      else
        Right(this)

    private[time] def isUnrestricted(local: LocalDateTime, dateOffset: JDuration): Boolean =
      months(local.minus(dateOffset).getMonthValue)

    private[time] def skipRestriction(local: LocalDateTime, dateOffset: JDuration): LocalDateTime =
      val localMinusOffset = local.minus(dateOffset)
      skippedMonths(localMinusOffset.getMonthValue) match
        case 0 => local
        case n =>
          localMinusOffset.toLocalDate.withDayOfMonth(1).plusMonths(n)
            .atStartOfDay
            .plus(dateOffset)

    private[time] def clipLocalInterval(localInterval: LocalInterval, dateOffset: JDuration)
    : Option[LocalInterval] =
      skippedMonths(localInterval.start.getMonthValue) match
        case 0 =>
          val monthOfStart = localInterval.start.minus(dateOffset).getMonthValue
          val monthOfEnd = localInterval.end.minus(dateOffset).getMonthValue
          if monthOfStart != monthOfEnd && !months(monthOfEnd) then
            // Next month is not allowed. Clip end lf localInterval!
            val end = MonthRestriction.startOfNextMonth(localInterval.start, dateOffset)
            val clippedSeconds = localInterval.end.toEpochSecond(UTC) - end.toEpochSecond(UTC)
            if clippedSeconds <= 0 then
              Some(localInterval) // No change
            else
              val duration = localInterval.duration - clippedSeconds.s
              duration.isPositive ? LocalInterval(localInterval.start, duration)
          else
            Some(localInterval)

        case n =>
          // This month is not allowed. Clip start of localInterval!
          val start = MonthRestriction.startOfNextMonth(localInterval.start, dateOffset)
          val skippedSeconds = start.toEpochSecond(UTC) - localInterval.start.toEpochSecond(UTC)
          val duration = localInterval.duration - skippedSeconds.s
          duration.isPositive ? LocalInterval(start, duration)

    private def skippedMonths(month: Int, findAllowed: Boolean = true): Int =
      val skipped =
        (month until month + 12).indexWhere: m =>
          months((m - 1) % 12 + 1) == findAllowed
      if skipped == -1 then throw new AssertionError("MonthRestriction.skipRestriction: No month")
      skipped

    override def toString: String =
      months.iterator.map(m => MonthNames(m - 1)).mkString("MonthRestriction(", " ", ")")


  private[time] object MonthRestriction:
    private[SchemeRestriction] def checked(months: Set[Int]): Checked[MonthRestriction] =
      new MonthRestriction(months.to(BitSet)).checked

    given Encoder.AsObject[MonthRestriction] = deriveEncoder

    given Decoder[MonthRestriction] =
      c => deriveCodec[MonthRestriction](c).flatMap(_.checked.toDecoderResult(c.history))

    private[time] val MonthNames =
      Vector("January", "February", "March", "April", "May", "June",
        "July", "August", "September", "October", "November", "December")

    private def startOfNextMonth(dt: LocalDateTime, dateOffset: JDuration): LocalDateTime =
      dt.minus(dateOffset).toLocalDate
        .plusMonths(1).withDayOfMonth(1)
        .atStartOfDay
        .plus(dateOffset)
