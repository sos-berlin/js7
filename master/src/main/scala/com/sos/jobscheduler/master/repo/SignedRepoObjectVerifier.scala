package com.sos.jobscheduler.master.repo

import cats.effect.{Resource, SyncIO}
import com.sos.jobscheduler.base.circeutils.CirceObjectCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.core.crypt.pgp.PgpSignatureVerifier
import com.sos.jobscheduler.data.crypt.{PgpSignature, SignerId}
import com.sos.jobscheduler.data.filebased.{FileBased, SignedRepoObject}
import java.io.{ByteArrayInputStream, InputStream}
import java.util.Base64
import scala.collection.immutable.Seq

final class SignedRepoObjectVerifier[A <: FileBased](verifier: PgpSignatureVerifier)
  (implicit jsonCodec: CirceObjectCodec[FileBased])
{
  def verifyAndDecodeSeq(signedRepoObjects: Seq[SignedRepoObject]): Checked[Vector[(FileBased, Seq[SignerId])]] =
    signedRepoObjects.failFastMap(verifyAndDecode)

  private def verifyAndDecode(signedRepoObject: SignedRepoObject): Checked[(FileBased, Seq[SignerId])] =
    verify(signedRepoObject).flatMap { case (message, userIds) ⇒
      message.parseJsonChecked
        .flatMap(json ⇒ jsonCodec.decodeJson(json).toChecked)
        .map(_ → userIds)
    }

  private def verify(signedRepoObject: SignedRepoObject): Checked[(String, Seq[SignerId])] =
    signedRepoObject match {
      case SignedRepoObject(string, PgpSignature(signature)) ⇒
        verifier.verifyString(string, base64ToInputStream(signature))
    }

  private def base64ToInputStream(base64: String) =
    Resource.fromAutoCloseable(SyncIO[InputStream] {
      new ByteArrayInputStream(Base64.getMimeDecoder.decode(base64))
    })
}
