package js7.base.time

import io.circe
import io.circe.generic.semiauto.{deriveCodec, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Codec, Decoder, Encoder, JsonObject}
import java.time.LocalTime.MIDNIGHT
import java.time.{LocalDateTime, Duration as JDuration}
import js7.base.circeutils.CirceUtils.toDecoderResult
import js7.base.circeutils.ScalaJsonCodecs.BitSetCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.problem.{Checked, Problem}
import js7.base.time.SchemeRestriction.MonthRestriction.MonthNames
import js7.base.time.SchemeRestriction.Unrestricted
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichEither}
import org.jetbrains.annotations.TestOnly
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
  def isUnrestricted(local: LocalDateTime, dateOffset: JDuration): Boolean
  def skipRestriction(local: LocalDateTime, dateOffset: JDuration): Option[LocalDateTime]


object SchemeRestriction:
  given TypedJsonCodec[SchemeRestriction] = TypedJsonCodec[SchemeRestriction](
    Subtype(Unrestricted),
    Subtype[MonthRestriction])


  object Unrestricted extends SchemeRestriction:
    def isUnrestricted(local: LocalDateTime, dateOffset: JDuration): Boolean =
      true

    def skipRestriction(local: LocalDateTime, dateOffset: JDuration) =
      Some(local)



  // MonthRestriction //

  /** Admission at specific months.
    *
    * Only the month of localInterval.start is checked
    * @param months 1..12. */
  final case class MonthRestriction private(months: BitSet) extends SchemeRestriction:
    def checked: Checked[this.type] =
      if !months.forall(m => m >= 1 && m <= 12) then
        Left(Problem.pure("Month must be a number between 1 and 12"))
      else
        Right(this)

    def isUnrestricted(local: LocalDateTime, dateOffset: JDuration): Boolean =
      months(local.minus(dateOffset).getMonthValue)

    def skipRestriction(local: LocalDateTime, dateOffset: JDuration): Option[LocalDateTime] =
      val localMinusOffset = local.minus(dateOffset)
      val month = localMinusOffset.getMonthValue
      (month until month + 12).indexWhere: m =>
        months((m - 1) % 12 + 1)
      match
        case -1 => None
        case 0 => Some(local)
        case skippedMonths => Some:
          LocalDateTime.of(
            localMinusOffset.toLocalDate.withDayOfMonth(1).plusMonths(skippedMonths),
            MIDNIGHT
          ).plus(dateOffset)

    override def toString: String =
      months.iterator.map(m => MonthNames(m - 1)).mkString("MonthRestriction(", " ", ")")


  object MonthRestriction:
    @TestOnly
    def apply(months: Set[Int]): MonthRestriction =
      new MonthRestriction(months.to(BitSet)).checked.orThrow

    @TestOnly
    def apply(months: Int*): MonthRestriction =
      new MonthRestriction(months.to(BitSet)).checked.orThrow

    def checked(months: Set[Int]): Checked[MonthRestriction] =
      new MonthRestriction(months.to(BitSet)).checked

    given Encoder.AsObject[MonthRestriction] = deriveEncoder

    given Decoder[MonthRestriction] =
      c => deriveCodec[MonthRestriction](c).flatMap(_.checked.toDecoderResult(c.history))

    private[time] val MonthNames =
      Vector("January", "February", "March", "April", "May", "June",
        "July", "August", "September", "October", "November", "December")
