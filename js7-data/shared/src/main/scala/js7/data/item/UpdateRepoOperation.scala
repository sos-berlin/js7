package js7.data.item

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.SignedString
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.controller.ControllerItems.itemPathJsonDecoder

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

  implicit val jsonCodec = TypedJsonCodec[UpdateRepoOperation](
    Subtype(deriveCodec[AddVersion]),
    Subtype(deriveCodec[AddOrReplace]),
    Subtype(deriveCodec[Delete]),
  )

  intelliJuseImport((itemPathJsonDecoder, TypedJsonCodec))
}
