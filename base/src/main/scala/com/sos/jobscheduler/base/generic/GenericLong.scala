package com.sos.jobscheduler.base.generic

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import io.circe.{Decoder, Encoder, Json}

/**
 * @author Joacim Zschimmer
 */
trait GenericLong {
  val number: Long
}

object GenericLong {

  trait Companion[A <: GenericLong] {
    def apply(o: Long): A

    implicit val JsonEncoder: Encoder[A] = o ⇒ Json.fromLong(o.number)
    implicit val JsonDecoder: Decoder[A] = o ⇒ Right(apply(o.value.forceLong))
  }
}
