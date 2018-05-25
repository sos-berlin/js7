package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec

/**
  * @author Joacim Zschimmer
  */
final case class OrdersOverview(
  count: Int)

object OrdersOverview {
  implicit val jsonCodec = deriveCodec[OrdersOverview]
}
