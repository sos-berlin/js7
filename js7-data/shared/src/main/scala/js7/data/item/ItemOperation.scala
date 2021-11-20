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
  sealed trait AddOrChangeOperation extends ItemOperation

  sealed trait Remove extends ItemOperation
  object Remove {
    def apply(path: InventoryItemPath): Remove =
      path match {
        case path: VersionedItemPath => ItemOperation.RemoveVersioned(path)
        case path: SimpleItemPath => ItemOperation.DeleteSimple(path)
      }
  }

  final case class AddOrChangeSimple(item: UnsignedSimpleItem)
  extends SimpleItemOperation with AddOrChangeOperation

  final case class DeleteSimple(path: SimpleItemPath)
  extends SimpleItemOperation with Remove

  sealed trait VersionedOperation extends ItemOperation
  object VersionedOperation
  {
    implicit def jsonCodec(implicit
      idJsonEncoder: Encoder[SimpleItemPath],
      idJsonDecoder: Decoder[SimpleItemPath],
      itemPathJsonEncoder: Encoder[VersionedItemPath],
      itemPathJsonDecoder: Decoder[VersionedItemPath])
    : TypedJsonCodec[VersionedOperation] =
      TypedJsonCodec(
        Subtype(deriveCodec[AddVersion]),
        Subtype(deriveCodec[AddOrChangeSigned]),
        Subtype(deriveCodec[RemoveVersioned]))
  }

  final case class AddVersion(versionId: VersionId)
  extends VersionedOperation

  sealed trait VersionedItemOperation extends VersionedOperation

  final case class AddOrChangeSigned(signedString: SignedString)
  extends VersionedItemOperation with AddOrChangeOperation

  final case class RemoveVersioned(path: VersionedItemPath)
  extends VersionedItemOperation with Remove

  implicit def jsonCodec(implicit
    idJsonEncoder: Encoder[SimpleItemPath],
    idJsonDecoder: Decoder[SimpleItemPath],
    itemPathJsonEncoder: Encoder[VersionedItemPath],
    itemPathJsonDecoder: Decoder[VersionedItemPath],
    unsignedSimpleItemJsonEncoder: Encoder[UnsignedSimpleItem],
    unsignedSimpleItemJsonDecoder: Decoder[UnsignedSimpleItem])
  : TypedJsonCodec[ItemOperation] =
    TypedJsonCodec(
      Subtype(deriveCodec[AddOrChangeSimple]),
      Subtype(deriveCodec[DeleteSimple]),
      Subtype[VersionedOperation])

  intelliJuseImport(signableItemJsonCodec)
}
