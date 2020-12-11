package js7.data.item

import io.circe.{Decoder, Encoder}
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.SignedString

sealed trait UpdateRepoOperation

object UpdateRepoOperation
{
  final case class AddVersion(versionId: VersionId)
  extends UpdateRepoOperation

  sealed trait ItemOperation extends UpdateRepoOperation

  final case class AddOrReplace(signedString: SignedString)
  extends ItemOperation

  final case class Delete(path: ItemPath)
  extends ItemOperation

  implicit def jsonCodec(implicit itemPathJsonEncoder: Encoder[ItemPath], itemPathJsonDecoder: Decoder[ItemPath])
  : TypedJsonCodec[UpdateRepoOperation] =
    TypedJsonCodec(
      Subtype(deriveCodec[AddVersion]),
      Subtype(deriveCodec[AddOrReplace]),
      Subtype(deriveCodec[Delete]))
}
