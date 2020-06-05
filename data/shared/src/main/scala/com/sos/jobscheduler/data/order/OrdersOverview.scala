package js7.data.order

import js7.base.circeutils.CirceUtils.deriveCodec

/**
  * @author Joacim Zschimmer
  */
final case class OrdersOverview(
  count: Int)

object OrdersOverview {
  implicit val jsonCodec = deriveCodec[OrdersOverview]
}
