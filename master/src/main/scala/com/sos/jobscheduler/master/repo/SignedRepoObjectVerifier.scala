package com.sos.jobscheduler.master.repo

import com.sos.jobscheduler.base.circeutils.CirceObjectCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.core.crypt.SignatureVerifier
import com.sos.jobscheduler.data.crypt.SignerId
import com.sos.jobscheduler.data.filebased.{FileBased, SignedRepoObject}
import scala.collection.immutable.Seq

final class SignedRepoObjectVerifier[A <: FileBased](verifier: SignatureVerifier)
  (implicit jsonCodec: CirceObjectCodec[FileBased])
{
  def verifyAndDecodeSeq(signedRepoObjects: Seq[SignedRepoObject]): Checked[Vector[(FileBased, Seq[SignerId])]] =
    signedRepoObjects.failFastMap(verifyAndDecode)

  private def verifyAndDecode(signedRepoObject: SignedRepoObject): Checked[(FileBased, Seq[SignerId])] =
    verify(signedRepoObject).flatMap(signerIds ⇒
      signedRepoObject.message.parseJsonChecked
        .flatMap(json ⇒ jsonCodec.decodeJson(json).toChecked)
        .map(_ → signerIds))

  private def verify(signedRepoObject: SignedRepoObject): Checked[Seq[SignerId]] =
    signedRepoObject match {
      case SignedRepoObject(string, genericSignature) ⇒
        verifier.verify(string, verifier.companion.genericSignatureToSignature(genericSignature))
    }
}
