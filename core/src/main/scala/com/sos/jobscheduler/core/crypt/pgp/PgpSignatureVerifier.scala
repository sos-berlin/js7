package com.sos.jobscheduler.core.crypt.pgp

import cats.data.Validated.{Invalid, Valid}
import cats.effect.{Resource, SyncIO}
import cats.syntax.show._
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.SyncResource.ops._
import com.sos.jobscheduler.common.scalautil.GuavaUtils.stringToInputStreamResource
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.utils.CatsUtils.bytesToInputStreamResource
import com.sos.jobscheduler.core.crypt.SignatureVerifier
import com.sos.jobscheduler.core.crypt.pgp.PgpCommons._
import com.sos.jobscheduler.core.problems.{PGPMessageSignedByUnknownProblem, PGPTamperedWithMessageProblem}
import com.sos.jobscheduler.data.crypt.{GenericSignature, PgpSignature, SignerId}
import java.io.InputStream
import org.bouncycastle.openpgp.jcajce.JcaPGPObjectFactory
import org.bouncycastle.openpgp.operator.jcajce.JcaPGPContentVerifierBuilderProvider
import org.bouncycastle.openpgp.{PGPPublicKeyRingCollection, PGPSignature, PGPSignatureList, PGPUtil}
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class PgpSignatureVerifier(publicKeyRingCollection: PGPPublicKeyRingCollection)
extends SignatureVerifier
{
  import com.sos.jobscheduler.core.crypt.pgp.PgpSignatureVerifier._

  protected type MySignature = PgpSignature
  def companion = PgpSignatureVerifier

  registerBouncyCastle()

  private val contentVerifierBuilderProvider = new JcaPGPContentVerifierBuilderProvider().setProvider("BC")

  /** Returns `Valid(message)` iff signature matches the message. */
  def verify(message: String, signature: PgpSignature): Checked[Seq[SignerId]] =
    verify(stringToInputStreamResource(message), stringToInputStreamResource(signature.string))

  /** Returns `Valid(userIds)` iff signature matches the message. */
  private def verify(message: Resource[SyncIO, InputStream], signature: Resource[SyncIO, InputStream]): Checked[Seq[SignerId]] =
    readMutableSignature(signature)
      .flatMap { sig ⇒
        logger.debug("Verifying with " + sig.show)
        publicKeyRingCollection.getPublicKey(sig.getKeyID) match {
          case null ⇒ Invalid(PGPMessageSignedByUnknownProblem)
          case publicKey ⇒
            sig.init(contentVerifierBuilderProvider, publicKey)
            Valid(sig)
        }
      }
      .flatMap { sig ⇒
        readMessage(message, sig.update(_, 0, _))
        if (sig.verify) Valid(signatureToUserIds(sig))
        else Invalid(PGPTamperedWithMessageProblem)
      }

  private def signatureToUserIds(signature: PGPSignature) =
    Option(publicKeyRingCollection.getPublicKey(signature.getKeyID)).toList
      .flatMap(_.getUserIDs.asScala)
      .map(SignerId.apply).toVector

  override def toString = s"PgpSignatureVerifier(userIds=${userIds.mkString("'", "', '", "'")} ${publicKeyRingCollection.show})"

  private def userIds: Seq[SignerId] =
    publicKeyRingCollection.asScala
      .flatMap(_.getPublicKeys.asScala)
      .filter(_.isMasterKey)
      .flatMap(_.getUserIDs.asScala)
      .map(SignerId.apply)
      .toVector
}

object PgpSignatureVerifier extends SignatureVerifier.Companion
{
  protected type MySignature = PgpSignature
  protected type MySignatureVerifier = PgpSignatureVerifier

  val typeName = PgpSignature.TypeName
  private val logger = Logger(getClass)

  def apply(keyRings: Seq[Byte]): PgpSignatureVerifier =
    apply(bytesToInputStreamResource(keyRings))

  def apply(keyRings: Resource[SyncIO, InputStream]): PgpSignatureVerifier =
    new PgpSignatureVerifier(readPublicKeyRingCollection(keyRings))

  def genericSignatureToSignature(signature: GenericSignature): PgpSignature = {
    assert(signature.typeName == typeName)
    PgpSignature(signature.string)
  }

  private[pgp] def readMutableSignature(in: Resource[SyncIO, InputStream]): Checked[PGPSignature] =
    in.useSync(in ⇒
      Checked.catchNonFatal(new JcaPGPObjectFactory(PGPUtil.getDecoderStream(in)).nextObject)
        .flatMap {
          case o: PGPSignatureList ⇒
            if (o.size != 1)
              Invalid(Problem(s"Unsupported PGP signature type: expected exactly one PGPSignature, not ${o.size}"))
            else
              Valid(o.get(0))

          case o ⇒
            logger.warn(s"Unsupported PGP signature type: ${o.getClass.getName} $o")
            Invalid(Problem("Unsupported PGP signature type"))
        })


  intelliJuseImport(PGPPublicKeyShow)
}
