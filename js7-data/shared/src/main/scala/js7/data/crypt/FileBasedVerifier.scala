package js7.data.crypt

import io.circe.Decoder
import js7.base.circeutils.CirceUtils.{RichCirceString, _}
import js7.base.crypt.{SignatureVerifier, Signed, SignedString, SignerId}
import js7.base.problem.Checked
import js7.data.crypt.FileBasedVerifier.Verified
import js7.data.filebased.FileBased

/**
  * @author Joacim Zschimmer
  */
final class FileBasedVerifier[A <: FileBased](signatureVerifier: SignatureVerifier, jsonDecoder: Decoder[A])
{
  def verify(signedString: SignedString): Checked[Verified[A]] =
    for {
      signers <- signatureVerifier.verify(signedString)
      json <- signedString.string.parseJsonChecked
      fileBased <- jsonDecoder.decodeJson(json).toChecked
    } yield Verified(Signed(fileBased, signedString), signers)
}

object FileBasedVerifier
{
  final case class Verified[A <: FileBased](signedFileBased: Signed[A], signerIds: Seq[SignerId]) {
    def fileBased: A = signedFileBased.value

    override def toString =
      s"'${fileBased.id}' verified: signed by ${signerIds.mkString("'", "', '", "'")}"
  }
}
