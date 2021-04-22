package js7.data.orderwatch

import js7.base.circeutils.CirceUtils.deriveCodec

final case class ExternalOrderKey(
  orderWatchPath: OrderWatchPath,
  name: ExternalOrderName)

object ExternalOrderKey
{
  implicit val jsonCodec = deriveCodec[ExternalOrderKey]
}
