package com.sos.jobscheduler.core.crypt.pgp

import cats.effect.{Resource, SyncIO}
import cats.syntax.show._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.SyncResource.syntax._
import com.sos.jobscheduler.common.scalautil.GuavaUtils.stringToInputStreamResource
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.crypt.SignatureVerifier
import com.sos.jobscheduler.core.crypt.pgp.PgpCommons.{RichPGPPublicKeyRingCollection, _}
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

  def keys = publicKeyRingCollection.toArmoredAsciiBytes :: Nil

  /** Returns `Right(message)` iff signature matches the message. */
  def verify(message: String, signature: PgpSignature): Checked[Seq[SignerId]] =
    verify(stringToInputStreamResource(message), stringToInputStreamResource(signature.string))

  /** Returns `Right(userIds)` iff signature matches the message. */
  private def verify(message: Resource[SyncIO, InputStream], signature: Resource[SyncIO, InputStream]): Checked[Seq[SignerId]] =
    for {
      pgpSignature <- readMutableSignature(signature)
      publicKey <- findPublicKeyInKeyRing(pgpSignature)
      signerIds <- verifyWithPublicKey(message, pgpSignature, publicKey)
    } yield signerIds

  private def findPublicKeyInKeyRing(signature: PGPSignature): Checked[PGPPublicKey] =
    publicKeyRingCollection.getPublicKey(signature.getKeyID) match {  // Public key is matched with the only 64-bit long key ID ???
      case null =>
        logger.debug(MessageSignedByUnknownProblem + ", no public key for " + signature.show)
        Left(MessageSignedByUnknownProblem)
      case publicKey =>
        Right(publicKey)
    }

  private def verifyWithPublicKey(message: Resource[SyncIO, InputStream], pgpSignature: PGPSignature, publicKey: PGPPublicKey): Checked[Seq[SignerId]] = {
    logger.trace("Verifying message with " + pgpSignature.show + ", using " + pgpPublicKeyToShortString(publicKey))
    pgpSignature.init(contentVerifierBuilderProvider, publicKey)
    readMessage(message, pgpSignature.update(_, 0, _))
    if (!pgpSignature.verify())
      Left(TamperedWithSignedMessageProblem)
    else
      Right(publicKey.getUserIDs.asScala.map(SignerId.apply).toVector)
  }

  override def toString = s"PgpSignatureVerifier(origin=$keyOrigin, ${publicKeyRingCollection.show})"
}

object PgpSignatureVerifier extends SignatureVerifier.Companion
{
  protected type MySignature = PgpSignature
  protected type MySignatureVerifier = PgpSignatureVerifier

  val typeName = PgpSignature.TypeName
  val recommendedKeyDirectoryName = "trusted-pgp-keys"
  val fileExtension = ".asc"

  private val logger = Logger(getClass)

  def checked(publicKeyRings: Seq[Resource[SyncIO, InputStream]], origin: String) =
    Checked.catchNonFatal(
      new PgpSignatureVerifier(readPublicKeyRingCollection(publicKeyRings), origin))

  def genericSignatureToSignature(signature: GenericSignature): PgpSignature = {
    assert(signature.typeName == typeName)
    PgpSignature(signature.signatureString)
  }

  private[pgp] def readMutableSignature(in: Resource[SyncIO, InputStream]): Checked[PGPSignature] =
    in.useSync(in =>
      Checked.catchNonFatal(new JcaPGPObjectFactory(PGPUtil.getDecoderStream(in)).nextObject)
        .flatMap {
          case o: PGPSignatureList =>
            if (o.size != 1)
              Left(Problem(s"Unsupported PGP signature type: expected exactly one PGPSignature, not ${o.size}"))
            else
              Right(o.get(0))

          case null =>
            Left(Problem("Not a valid PGP signature"))

          case o =>
            logger.warn(s"Unsupported PGP signature type: ${o.getClass.getName} $o")
            Left(Problem("Unsupported PGP signature type"))
        })


  intelliJuseImport(PGPPublicKeyShow)
}
