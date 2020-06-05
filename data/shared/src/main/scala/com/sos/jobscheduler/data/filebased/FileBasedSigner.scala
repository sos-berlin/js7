package js7.data.filebased

import js7.base.circeutils.CirceUtils.RichJson
import js7.base.crypt.{MessageSigner, Signed, SignedString}
import js7.data.filebased.RepoEvent.{FileBasedAdded, FileBasedChanged}
import io.circe.Encoder

/**
  * @author Joacim Zschimmer
  */
final class FileBasedSigner[A <: FileBased](val signer: MessageSigner, jsonEncoder: Encoder[A])
{
  def toSigned(fileBased: A): Signed[A] =
    js7.base.crypt.Signed(fileBased, sign(fileBased))

  def sign(fileBased: A): SignedString = {
    val string = jsonEncoder(fileBased).compactPrint
    SignedString(string, signer.sign(string).toGenericSignature)
  }

  def toAddedEvent(fileBased: A): FileBasedAdded =
    FileBasedAdded(fileBased.path, sign(fileBased))

  def toChangedEvent(fileBased: A): FileBasedChanged =
    FileBasedChanged(fileBased.path, sign(fileBased))
}
