package js7.data.item

import io.circe.Encoder
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.crypt.{DocumentSigner, Signed, SignedString}
import js7.data.item.RepoEvent.{ItemAdded, ItemChanged}

/**
  * @author Joacim Zschimmer
  */
final class VersionedItemSigner[A <: VersionedItem](val signer: DocumentSigner, jsonEncoder: Encoder[A])
{
  def toSigned(item: A): Signed[A] =
    toSigned_(item)

  private def toSigned_[B >: A <: VersionedItem](item: A): Signed[B] =
    js7.base.crypt.Signed(item, sign(item))

  def sign(item: A): SignedString = {
    val string = jsonEncoder(item).compactPrint
    SignedString(string, signer.signString(string).toGenericSignature)
  }

  def toAddedEvent(item: A): ItemAdded =
    ItemAdded(toSigned_(item))

  def toChangedEvent(item: A): ItemChanged =
    ItemChanged(toSigned_(item))
}
