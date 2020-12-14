package js7.data.lock

import io.circe.{Decoder, Encoder}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.utils.Assertions.assertThat
import js7.data.item.SimpleItem

final case class Lock(id: LockId, limit: Int) extends SimpleItem
{
  protected type Self = Lock
  val companion = Lock

  assertThat(limit >= 0)
}

object Lock extends SimpleItem.Companion
{
  type Item = Lock
  type Id = LockId

  val jsonCodec = deriveCodec[Lock]

  implicit val jsonEncoder: Encoder.AsObject[Lock] = jsonCodec
  implicit val jsonDecoder: Decoder[Lock] = jsonCodec

  val idCompanion = LockId
}
