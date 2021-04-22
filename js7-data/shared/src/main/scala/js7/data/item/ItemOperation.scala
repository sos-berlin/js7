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

  final case class AddOrChangeSimple(item: UnsignedSimpleItem)
  extends SimpleItemOperation

  final case class DeleteSimple(id: SimpleItemPath)
  extends SimpleItemOperation

  sealed trait VersionedOperation extends ItemOperation
  object VersionedOperation
  {
    implicit def jsonCodec(implicit
      idJsonEncoder: Encoder[SimpleItemPath],
      idJsonDecoder: Decoder[SimpleItemPath],
      itemPathJsonEncoder: Encoder[ItemPath],
      itemPathJsonDecoder: Decoder[ItemPath])
    : TypedJsonCodec[VersionedOperation] =
      TypedJsonCodec(
        Subtype(deriveCodec[AddVersion]),
        Subtype(deriveCodec[AddOrChangeSigned]),
        Subtype(deriveCodec[DeleteVersioned]))
  }

  final case class AddVersion(versionId: VersionId)
  extends VersionedOperation

  sealed trait VersionedItemOperation extends VersionedOperation

  final case class AddOrChangeSigned(signedString: SignedString)
  extends VersionedItemOperation

  final case class DeleteVersioned(path: ItemPath)
  extends VersionedItemOperation

  implicit def jsonCodec(implicit
    idJsonEncoder: Encoder[SimpleItemPath],
    idJsonDecoder: Decoder[SimpleItemPath],
    itemPathJsonEncoder: Encoder[ItemPath],
    itemPathJsonDecoder: Decoder[ItemPath],
    unsignedSimpleItemJsonEncoder: Encoder[UnsignedSimpleItem],
    unsignedSimpleItemJsonDecoder: Decoder[UnsignedSimpleItem])
  : TypedJsonCodec[ItemOperation] =
    TypedJsonCodec(
      Subtype(deriveCodec[AddOrChangeSimple]),
      Subtype(deriveCodec[DeleteSimple]),
      Subtype[VersionedOperation])

  intelliJuseImport(signableItemJsonCodec)
}
