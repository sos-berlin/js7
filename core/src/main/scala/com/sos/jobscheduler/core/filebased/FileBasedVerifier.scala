package com.sos.jobscheduler.core.filebased

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichCirceString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.core.crypt.SignatureVerifier
import com.sos.jobscheduler.core.filebased.FileBasedVerifier._
import com.sos.jobscheduler.data.crypt.{Signed, SignedString, SignerId}
import com.sos.jobscheduler.data.filebased.FileBased
import io.circe.Decoder
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class FileBasedVerifier(signatureVerifier: SignatureVerifier, jsonDecoder: Decoder[FileBased])
{
  def verify(signedString: SignedString): Checked[Verified] =
    for {
      signers ← signatureVerifier.verify(signedString)
      json ← signedString.string.parseJsonChecked
      fileBased ← jsonDecoder.decodeJson(json).toSimpleChecked
    } yield Verified(Signed(fileBased, signedString), signers)
}

object FileBasedVerifier {
  final case class Verified(signedFileBased: Signed[FileBased], signerIds: Seq[SignerId]) {
    def fileBased: FileBased = signedFileBased.value
  }
}
