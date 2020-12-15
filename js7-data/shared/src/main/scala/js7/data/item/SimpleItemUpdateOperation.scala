package js7.data.item

import io.circe.{Decoder, Encoder}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}

sealed trait SimpleItemUpdateOperation

object SimpleItemUpdateOperation
{
  sealed trait ItemOperation extends SimpleItemUpdateOperation

  final case class AddOrChange(item: SimpleItem)
  extends ItemOperation

  final case class Delete(id: SimpleItemId)
  extends ItemOperation

  implicit def jsonCodec(implicit
    simpleItemJsonEncoder: Encoder[SimpleItem],
    simpleItemJsonDecoder: Decoder[SimpleItem],
    idJsonEncoder: Encoder[SimpleItemId],
    itemPathJsonDecoder: Decoder[SimpleItemId])
  : TypedJsonCodec[SimpleItemUpdateOperation] =
    TypedJsonCodec(
      Subtype(deriveCodec[AddOrChange]),
      Subtype(deriveCodec[Delete]))
}
