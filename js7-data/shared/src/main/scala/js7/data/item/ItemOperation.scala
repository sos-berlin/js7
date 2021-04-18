package js7.data.item

import io.circe.{Decoder, Encoder}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.SignedString
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.controller.ControllerState.signableItemJsonCodec

sealed trait ItemOperation

object ItemOperation
{
  sealed trait SimpleItemOperation extends ItemOperation

  final case class SimpleAddOrChange(item: UnsignedSimpleItem)
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
        Subtype(deriveCodec[SignedAddOrChange]),
        Subtype(deriveCodec[VersionedDelete]))
  }

  final case class AddVersion(versionId: VersionId)
  extends VersionedOperation

  sealed trait VersionedItemOperation extends VersionedOperation

  final case class SignedAddOrChange(signedString: SignedString)
  extends VersionedItemOperation

  final case class VersionedDelete(path: ItemPath)
  extends VersionedItemOperation

  implicit def jsonCodec(implicit
    idJsonEncoder: Encoder[SimpleItemId],
    idJsonDecoder: Decoder[SimpleItemId],
    itemPathJsonEncoder: Encoder[ItemPath],
    itemPathJsonDecoder: Decoder[ItemPath],
    unsignedSimpleItemJsonEncoder: Encoder[UnsignedSimpleItem],
    unsignedSimpleItemJsonDecoder: Decoder[UnsignedSimpleItem])
  : TypedJsonCodec[ItemOperation] =
    TypedJsonCodec(
      Subtype(deriveCodec[SimpleAddOrChange]),
      Subtype(deriveCodec[SimpleDelete]),
      Subtype[VersionedOperation])

  intelliJuseImport(signableItemJsonCodec)
}
