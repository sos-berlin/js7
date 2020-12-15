package js7.data.item

import io.circe.{Decoder, Encoder}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.SignedString

sealed trait ItemOperation

object ItemOperation
{
  sealed trait SimpleItemOperation extends ItemOperation

  final case class SimpleAddOrChange(item: SimpleItem)
  extends SimpleItemOperation

  final case class SimpleDelete(id: SimpleItemId)
  extends SimpleItemOperation

  sealed trait VersionedOperation extends ItemOperation
  object VersionedOperation
  {
    implicit def jsonCodec(implicit
      idJsonEncoder: Encoder[SimpleItemId],
      idJsonDecoder: Decoder[SimpleItemId],
      itemPathJsonEncoder: Encoder[ItemPath],
      itemPathJsonDecoder: Decoder[ItemPath])
    : TypedJsonCodec[VersionedOperation] =
      TypedJsonCodec(
        Subtype(deriveCodec[AddVersion]),
        Subtype(deriveCodec[VersionedAddOrChange]),
        Subtype(deriveCodec[VersionedDelete]))
  }

  final case class AddVersion(versionId: VersionId)
  extends VersionedOperation

  sealed trait VersionedItemOperation extends VersionedOperation

  final case class VersionedAddOrChange(signedString: SignedString)
  extends VersionedItemOperation

  final case class VersionedDelete(path: ItemPath)
  extends VersionedItemOperation

  implicit def jsonCodec(implicit
    idJsonEncoder: Encoder[SimpleItemId],
    idJsonDecoder: Decoder[SimpleItemId],
    itemPathJsonEncoder: Encoder[ItemPath],
    itemPathJsonDecoder: Decoder[ItemPath],
    simpleItemJsonEncoder: Encoder[SimpleItem],
    simpleItemJsonDecoder: Decoder[SimpleItem])
  : TypedJsonCodec[ItemOperation] =
    TypedJsonCodec(
      Subtype(deriveCodec[SimpleAddOrChange]),
      Subtype(deriveCodec[SimpleDelete]),
      Subtype[VersionedOperation])
}
