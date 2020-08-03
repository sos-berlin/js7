package js7.data.filebased

import io.circe.Encoder
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.crypt.{MessageSigner, Signed, SignedString}
import js7.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged}

/**
  * @author Joacim Zschimmer
  */
final class FileBasedSigner[A <: FileBased](val signer: MessageSigner, jsonEncoder: Encoder[A])
{
  def toSigned(fileBased: A): Signed[A] =
    toSigned_(fileBased)

  private def toSigned_[B >: A <: FileBased](fileBased: A): Signed[B] =
    js7.base.crypt.Signed(fileBased, sign(fileBased))

  def sign(fileBased: A): SignedString = {
    val string = jsonEncoder(fileBased).compactPrint
    SignedString(string, signer.sign(string).toGenericSignature)
  }

  def toAddedEvent(fileBased: A): FileBasedAdded =
    FileBasedAdded(toSigned_(fileBased))

  def toChangedEvent(fileBased: A): FileBasedChanged =
    FileBasedChanged(toSigned_(fileBased))
}
