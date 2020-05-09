package com.sos.jobscheduler.data.crypt

import com.sos.jobscheduler.base.circeutils.CirceUtils.{RichCirceString, _}
import com.sos.jobscheduler.base.crypt.{SignatureVerifier, Signed, SignedString, SignerId}
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.data.crypt.FileBasedVerifier.Verified
import com.sos.jobscheduler.data.filebased.FileBased
import io.circe.Decoder

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
