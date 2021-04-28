package js7.data.item

import io.circe.Encoder
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.crypt.{DocumentSigner, Signed, SignedString}

final class ItemSigner[A <: SignableItem](val signer: DocumentSigner, jsonEncoder: Encoder[A])
{
  def sign[A1 <: A](item: A1): Signed[A1] =
    toSigned_(item)

  private def toSigned_[A1 <: A](item: A1): Signed[A1] =
    item match {
      case simpleItem: SignableSimpleItem =>
        Signed(item, toSignedString(simpleItem.withRevision(None).asInstanceOf[A1]))

      case item: VersionedItem with (A1 @unchecked) =>
        Signed(item, toSignedString(item))
    }

  def toSignedString[A1 <: A](item: A1): SignedString =
    signer.toSignedString(jsonEncoder(item).compactPrint)
}
