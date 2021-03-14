package js7.data.orderwatch

import js7.base.circeutils.CirceUtils.deriveCodec

final case class ExternalOrderKey(
  orderWatchId: OrderWatchId,
  name: ExternalOrderName)

object ExternalOrderKey
{
  implicit val jsonCodec = deriveCodec[ExternalOrderKey]
}
