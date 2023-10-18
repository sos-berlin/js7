package js7.base.generic

import io.circe.{Decoder, Encoder, Json}

/**
 * @author Joacim Zschimmer
 */
trait GenericLong:
  val number: Long


object GenericLong:

  trait Companion[A <: GenericLong]:
    def apply(o: Long): A

    implicit val JsonEncoder: Encoder[A] = o => Json.fromLong(o.number)
    implicit val JsonDecoder: Decoder[A] = _.as[Long] map apply
