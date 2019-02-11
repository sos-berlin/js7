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
import com.sos.jobscheduler.core.problems.{MessageSignedByUnknownProblem, TamperedWithSignedMessageProblem}
import com.sos.jobscheduler.data.crypt.{GenericSignature, PgpSignature, SignerId}
import java.io.InputStream
import org.bouncycastle.openpgp.jcajce.JcaPGPObjectFactory
import org.bouncycastle.openpgp.operator.jcajce.JcaPGPContentVerifierBuilderProvider
import org.bouncycastle.openpgp.{PGPPublicKey, PGPPublicKeyRingCollection, PGPSignature, PGPSignatureList, PGPUtil}
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class PgpSignatureVerifier(publicKeyRingCollection: PGPPublicKeyRingCollection, val keyOrigin: String)
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
    for {
      pgpSignature <- readMutableSignature(signature)
      publicKey <- findPublicKeyInKeyRing(pgpSignature)
      signerIds <- verifyWithPublicKey(message, pgpSignature, publicKey)
    } yield signerIds

  private def findPublicKeyInKeyRing(signature: PGPSignature): Checked[PGPPublicKey] =
    publicKeyRingCollection.getPublicKey(signature.getKeyID) match {  // Public key is matched with the only 64-bit long key ID ???
      case null ⇒
        logger.debug(MessageSignedByUnknownProblem + ", no public key for " + signature.show)
        Invalid(MessageSignedByUnknownProblem)
      case publicKey ⇒
        Valid(publicKey)
    }

  private def verifyWithPublicKey(message: Resource[SyncIO, InputStream], pgpSignature: PGPSignature, publicKey: PGPPublicKey): Checked[Seq[SignerId]] = {
    logger.debug("Verifying message with " + pgpSignature.show + ", using " + publicKey.show)
    pgpSignature.init(contentVerifierBuilderProvider, publicKey)
    readMessage(message, pgpSignature.update(_, 0, _))
    if (!pgpSignature.verify())
      Invalid(TamperedWithSignedMessageProblem)
    else
      Valid(publicKey.getUserIDs.asScala.map(SignerId.apply).toVector)
  }

  override def toString = s"PgpSignatureVerifier(origin=$keyOrigin, ${publicKeyRingCollection.show})"
}

object PgpSignatureVerifier extends SignatureVerifier.Companion
{
  protected type MySignature = PgpSignature
  protected type MySignatureVerifier = PgpSignatureVerifier

  val typeName = PgpSignature.TypeName
  private val logger = Logger(getClass)

  def checked(keyRings: Seq[Byte], keyOrigin: String) =
    Checked.catchNonFatal(
      apply(bytesToInputStreamResource(keyRings), keyOrigin = keyOrigin))

  private def apply(keyRings: Resource[SyncIO, InputStream], keyOrigin: String): PgpSignatureVerifier =
    new PgpSignatureVerifier(readPublicKeyRingCollection(keyRings), keyOrigin)

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
