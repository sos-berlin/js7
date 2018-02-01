package com.sos.jobscheduler.base.generic

import com.sos.jobscheduler.base.convert.As
import io.circe.{Decoder, Encoder, Json}

/**
  * @author Joacim Zschimmer
  */
trait GenericInt {
  def number: Int
}

object GenericInt {
  trait Companion[A <: GenericInt] {
    def apply(number: Int): A

    implicit val ordering: Ordering[A] = Ordering by { _.number }

    implicit def stringAsGenericInt: As[String, A] =
      As(o ⇒ apply(o.toInt))

    implicit val JsonEncoder: Encoder[A] = o ⇒ Json.fromInt(o.number)
    implicit val JsonDecoder: Decoder[A] = _.as[Int] map apply
  }
}
