package com.sos.jobscheduler.core.filebased

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichCirceString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.crypt.SignatureVerifier
import com.sos.jobscheduler.core.filebased.FileBasedVerifier._
import com.sos.jobscheduler.data.crypt.{Signed, SignedString}
import com.sos.jobscheduler.data.filebased.FileBased
import io.circe.Decoder

/**
  * @author Joacim Zschimmer
  */
final class FileBasedVerifier(signatureVerifier: SignatureVerifier, jsonDecoder: Decoder[FileBased])
{
  def verify(signedString: SignedString): Checked[Signed[FileBased]] =
    for {
      signers ← signatureVerifier.verify(signedString.string,
        signatureVerifier.companion.genericSignatureToSignature(signedString.signature))
      json ← signedString.string.parseJsonChecked
      fileBased ←  jsonDecoder.decodeJson(json).toSimpleChecked
    } yield {
      logger.info(s"Configuration object '${fileBased.id}' verified, signed by ${signers.mkString("'", "', '", "'")}")
      Signed(fileBased, signedString)
    }
}

object FileBasedVerifier
{
  private val logger = Logger(getClass)
}
