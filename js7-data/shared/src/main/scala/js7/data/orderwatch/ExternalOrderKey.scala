package js7.data.orderwatch

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

final case class ExternalOrderKey(
  orderWatchPath: OrderWatchPath,
  name: ExternalOrderName)


object ExternalOrderKey:
  implicit val jsonCodec: Codec.AsObject[ExternalOrderKey] = deriveCodec
