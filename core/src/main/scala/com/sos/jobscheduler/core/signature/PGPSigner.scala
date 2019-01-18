package com.sos.jobscheduler.core.signature

import cats.effect.{Resource, SyncIO}
import cats.instances.vector._
import cats.syntax.foldable.catsSyntaxFoldOps
import cats.syntax.show._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.utils.SyncResource.ops.RichResource
import com.sos.jobscheduler.core.signature.PGPCommons._
import com.sos.jobscheduler.core.signature.PGPSigner.newSignatureGenerator
import java.io.{ByteArrayOutputStream, InputStream}
import org.bouncycastle.bcpg.{BCPGOutputStream, HashAlgorithmTags}
import org.bouncycastle.openpgp.operator.jcajce.{JcaKeyFingerprintCalculator, JcaPGPContentSignerBuilder, JcePBESecretKeyDecryptorBuilder}
import org.bouncycastle.openpgp.{PGPSecretKey, PGPSecretKeyRingCollection, PGPSignature, PGPSignatureGenerator, PGPSignatureSubpacketGenerator, PGPUtil}
import scala.collection.JavaConverters._

/**
  * @author Joacim Zschimmer
  */
final class PGPSigner(pgpSecretKey: PGPSecretKey, password: SecretString)
{
  //logger.debug(pgpSecretKey.show)

  def sign(message: Resource[SyncIO, InputStream]): Array[Byte] = {
    val signatureGenerator = newSignatureGenerator(pgpSecretKey, password)
    readMessage(message, signatureGenerator.update(_, 0, _))
    finish(signatureGenerator)
  }

  private def finish(signatureGenerator: PGPSignatureGenerator): Array[Byte] = {
    val byteOut = new ByteArrayOutputStream
    val bOut = new BCPGOutputStream(byteOut)
    signatureGenerator.generate.encode(bOut)
    bOut.close()
    byteOut.toByteArray
  }

  override def toString = show"PGPSigner($pgpSecretKey)"
}

object PGPSigner
{
  private val OurHashAlgorithm = HashAlgorithmTags.SHA512

  registerBountyCastle()

  def apply(secretKey: Resource[SyncIO, InputStream], password: SecretString): PGPSigner = {
    val keyRings = readKeyRingCollection(secretKey)
    //logger.debug(keyRings.show)
    new PGPSigner(selectSecretKey(keyRings), password)
  }

  private def selectSecretKey(keyRings: PGPSecretKeyRingCollection): PGPSecretKey = {
    val keys = keyRings
      .getKeyRings.asScala
      .map(o ⇒ o.getSecretKey(o.getPublicKey/*the master key*/.getFingerprint))
      .toVector
    if (keys.isEmpty) throw new NoSuchElementException("No master key in secret key ring")
    if (keys.size > 1) throw new IllegalArgumentException(s"More than one master key in secret key ring: " + keys.mkString_("", ", ", ""))
    keys.head
  }

  private def readKeyRingCollection(secretKey: Resource[SyncIO, InputStream]): PGPSecretKeyRingCollection =
    secretKey.useSync(in ⇒
      new PGPSecretKeyRingCollection(
        PGPUtil.getDecoderStream(in),
        new JcaKeyFingerprintCalculator/*or BcKeyFingerprintCalculator?*/))

  private def newSignatureGenerator(secretKey: PGPSecretKey, password: SecretString): PGPSignatureGenerator = {
    val privateKey = secretKey.extractPrivateKey(
      new JcePBESecretKeyDecryptorBuilder() //?new JcaPGPDigestCalculatorProviderBuilder().setProvider("BC").build)
        .setProvider("BC")
        .build(password.string.toArray))
    val signatureGenerator = new PGPSignatureGenerator(
      new JcaPGPContentSignerBuilder(secretKey.getPublicKey.getAlgorithm, OurHashAlgorithm).setProvider("BC"))
    signatureGenerator.init(PGPSignature.BINARY_DOCUMENT, privateKey)
    val subpacketGenerator = new PGPSignatureSubpacketGenerator
    val it = secretKey.getPublicKey.getUserIDs
    if (it.hasNext) {
      subpacketGenerator.setSignerUserID(false, it.next())
      signatureGenerator.setHashedSubpackets(subpacketGenerator.generate)
    }
    signatureGenerator
  }
}

