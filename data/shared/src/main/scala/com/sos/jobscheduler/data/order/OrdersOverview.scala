package com.sos.jobscheduler.data.order

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec

/**
  * @author Joacim Zschimmer
  */
final case class OrdersOverview(
  orderCount: Int)

object OrdersOverview {
  implicit val jsonCodec = deriveCirceCodec[OrdersOverview]
}
