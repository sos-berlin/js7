package com.sos.jobscheduler.core.crypt.pgp

import cats.effect.{Resource, SyncIO}
import cats.instances.vector._
import cats.syntax.foldable.catsSyntaxFoldOps
import cats.syntax.show._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.utils.SyncResource.ops.RichResource
import com.sos.jobscheduler.common.scalautil.GuavaUtils.stringToInputStreamResource
import com.sos.jobscheduler.common.utils.CatsUtils.bytesToInputStreamResource
import com.sos.jobscheduler.core.crypt.MessageSigner
import com.sos.jobscheduler.core.crypt.pgp.PgpCommons._
import com.sos.jobscheduler.data.crypt.PgpSignature
import java.io.InputStream
import java.util.Base64
import org.bouncycastle.bcpg.HashAlgorithmTags
import org.bouncycastle.openpgp.operator.jcajce.{JcaPGPContentSignerBuilder, JcePBESecretKeyDecryptorBuilder}
import org.bouncycastle.openpgp.{PGPSecretKey, PGPSecretKeyRingCollection, PGPSignature, PGPSignatureGenerator, PGPSignatureSubpacketGenerator, PGPUtil}
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class PgpSigner private(pgpSecretKey: PGPSecretKey, password: SecretString)
extends MessageSigner
{
  protected type MySignature = PgpSignature

  def companion = PgpSigner

  import PgpSigner._

  registerBouncyCastle()
  //logger.debug(pgpSecretKey.show)

  private val pgpPrivateKey = pgpSecretKey.extractPrivateKey(
    new JcePBESecretKeyDecryptorBuilder() //?new JcaPGPDigestCalculatorProviderBuilder()
      .setProvider("BC")
      .build(password.string.toArray))
  private val maybeUserId: Option[String] = pgpSecretKey.getPublicKey.getUserIDs.asScala.buffered.headOption  // Only the first UserID ?

  def sign(message: String): PgpSignature = {
    val signatureBytes = sign(stringToInputStreamResource(message))
    PgpSignature(Base64.getMimeEncoder.encodeToString(signatureBytes))
  }

  /** The private key in armored ASCII. */
  def privateKey: Seq[Byte] =
    pgpSecretKey.toArmoredAsciiBytes

  /** The public key in armored ASCII. */
  def publicKey: Seq[Byte] =
    pgpSecretKey.getPublicKey.toArmoredAsciiBytes

  private def sign(message: Resource[SyncIO, InputStream]): Array[Byte] = {
    val signatureGenerator = newSignatureGenerator()
    readMessage(message, signatureGenerator.update(_, 0, _))
    finish(signatureGenerator)
  }

  private def newSignatureGenerator(): PGPSignatureGenerator = {
    val signatureGenerator = new PGPSignatureGenerator(
      new JcaPGPContentSignerBuilder(pgpSecretKey.getPublicKey.getAlgorithm, OurHashAlgorithm)
        .setProvider("BC"))
    signatureGenerator.init(PGPSignature.BINARY_DOCUMENT, pgpPrivateKey)
    for (u ← maybeUserId) {
      val subpacketGenerator = new PGPSignatureSubpacketGenerator
      subpacketGenerator.setSignerUserID(false, u)
      signatureGenerator.setHashedSubpackets(subpacketGenerator.generate())
    }
    signatureGenerator
  }

  private def finish(signatureGenerator: PGPSignatureGenerator): Array[Byte] =
    signatureGenerator.generate.getEncoded(/*forTransfer=*/true)

  def toVerifier =
    PgpSignatureVerifier.checked(bytesToInputStreamResource(publicKey), keyOrigin = "PgpSigner").orThrow

  override def toString = show"PgpSigner($pgpSecretKey)"
}

object PgpSigner extends MessageSigner.Companion
{
  protected type MySignature = PgpSignature
  protected type MyMessageSigner = PgpSigner

  def typeName = PgpSignature.TypeName

  def checked(privateKey: collection.Seq[Byte], password: SecretString) =
    Checked.catchNonFatal(
      new PgpSigner(readSecretKey(bytesToInputStreamResource(privateKey)), password))

  private val OurHashAlgorithm = HashAlgorithmTags.SHA512

  def apply(pgpSecretKey: PGPSecretKey, password: SecretString): Checked[PgpSigner] =
    Checked.catchNonFatal(
      new PgpSigner(pgpSecretKey, password))

  def readSecretKey(resource: Resource[SyncIO, InputStream]): PGPSecretKey =
    selectSecretKey(readSecretKeyRingCollection(resource))

  private def readSecretKeyRingCollection(resource: Resource[SyncIO, InputStream]): PGPSecretKeyRingCollection =
    resource.useSync(in ⇒
      new PGPSecretKeyRingCollection(PGPUtil.getDecoderStream(in), newFingerPrintCalculator))

  private def selectSecretKey(keyRings: PGPSecretKeyRingCollection): PGPSecretKey = {
    val keys = keyRings
      .getKeyRings.asScala
      .map(o ⇒ o.getSecretKey(o.getPublicKey/*the master key*/.getFingerprint))
      .toVector
    if (keys.isEmpty) throw new NoSuchElementException("No master key in secret key ring")
    if (keys.size > 1) throw new IllegalArgumentException(s"More than one master key in secret key ring: " + keys.mkString_("", ", ", ""))
    keys.head
  }
}

