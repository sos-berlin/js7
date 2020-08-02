package js7.data.filebased

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.crypt.SignedString
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.controller.ControllerFileBaseds.typedPathJsonDecoder

sealed trait UpdateRepoOperation

object UpdateRepoOperation
{
  final case class AddVersion(versionId: VersionId)
  extends UpdateRepoOperation

  sealed trait ObjectOperation extends UpdateRepoOperation

  final case class AddOrReplace(signedString: SignedString)
  extends ObjectOperation

  final case class Delete(path: TypedPath)
  extends ObjectOperation

  implicit val jsonCodec = TypedJsonCodec[UpdateRepoOperation](
    Subtype(deriveCodec[AddVersion]),
    Subtype(deriveCodec[AddOrReplace]),
    Subtype(deriveCodec[Delete]),
  )

  intelliJuseImport((typedPathJsonDecoder, TypedJsonCodec))
}
