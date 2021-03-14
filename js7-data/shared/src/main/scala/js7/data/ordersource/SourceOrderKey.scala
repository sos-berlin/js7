package js7.data.ordersource

import js7.base.circeutils.CirceUtils.deriveCodec

final case class SourceOrderKey(
  orderSourceId: OrderSourceId,
  name: SourceOrderName)

object SourceOrderKey
{
  implicit val jsonCodec = deriveCodec[SourceOrderKey]
}
