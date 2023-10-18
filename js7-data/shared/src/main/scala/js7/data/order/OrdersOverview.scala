package js7.data.order

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

/**
  * @author Joacim Zschimmer
  */
final case class OrdersOverview(
  count: Int)


object OrdersOverview:
  implicit val jsonCodec: Codec.AsObject[OrdersOverview] = deriveCodec[OrdersOverview]
