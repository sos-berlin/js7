package js7.base.generic

import io.circe.{Decoder, Encoder, Json}
import js7.base.convert.As
import js7.base.utils.ScalaUtils.syntax.*

/**
  * @author Joacim Zschimmer
  */
trait GenericInt {
  def number: Int
}

object GenericInt {
  trait Companion[A <: GenericInt] {
    val name = getClass.simpleScalaName

    def apply(number: Int): A

    implicit val self = this

    implicit val ordering: Ordering[A] = Ordering.by(_.number)

    implicit def stringAsGenericInt: As[String, A] =
      As(o => apply(o.toInt))

    implicit val JsonEncoder: Encoder[A] = o => Json.fromInt(o.number)
    implicit val JsonDecoder: Decoder[A] = _.as[Int] map apply
  }
}
