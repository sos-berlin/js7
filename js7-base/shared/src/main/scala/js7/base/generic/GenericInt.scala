package js7.base.generic

import cats.parse.Parser
import io.circe.{Decoder, Encoder, Json}
import js7.base.convert.As
import js7.base.parser.BasicParsers
import js7.base.utils.Ordinal
import js7.base.utils.ScalaUtils.syntax.*

/**
  * @author Joacim Zschimmer
  */
trait GenericInt:
  def number: Int


object GenericInt:
  trait Companion[A <: GenericInt]:
    val name: String = getClass.shortClassName

    def apply(number: Int): A

    implicit val self: Companion[A] = this

    implicit val ordering: Ordering[A] =
      Ordering.by(_.number)

    implicit val ordinal: Ordinal[A] =
      new Ordinal[A]:
        def succ(a: A) = apply(a.number + 1)

        def pred(a: A) = apply(a.number - 1)

        /** a isSuccessorOf b. */
        override def isSuccessorOf(a: A, b: A): Boolean =
          a.number == b.number + 1
    implicit def stringAsGenericInt: As[String, A] =
      As(o => apply(o.toInt))

    // Required for RangeSet[ReturnCode].jsonDecoder
    implicit lazy val parser: Parser[A] =
      BasicParsers.int.map(apply(_))

    implicit val JsonEncoder: Encoder[A] = o => Json.fromInt(o.number)
    implicit val JsonDecoder: Decoder[A] = _.as[Int] map apply
