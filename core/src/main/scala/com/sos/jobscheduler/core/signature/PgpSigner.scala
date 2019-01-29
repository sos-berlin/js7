package com.sos.jobscheduler.core.signature

import cats.effect.{Resource, SyncIO}
import cats.instances.vector._
import cats.syntax.foldable.catsSyntaxFoldOps
import cats.syntax.show._
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.utils.SyncResource.ops.RichResource
import com.sos.jobscheduler.core.signature.PgpCommons._
import com.sos.jobscheduler.core.signature.PgpSigner.newSignatureGenerator
import java.io.{ByteArrayOutputStream, InputStream, OutputStream}
import org.bouncycastle.bcpg.{ArmoredOutputStream, BCPGOutputStream, HashAlgorithmTags}
import org.bouncycastle.openpgp.operator.jcajce.{JcaPGPContentSignerBuilder, JcePBESecretKeyDecryptorBuilder}
import org.bouncycastle.openpgp.{PGPSecretKey, PGPSecretKeyRing, PGPSecretKeyRingCollection, PGPSignature, PGPSignatureGenerator, PGPSignatureSubpacketGenerator, PGPUtil}
import scala.collection.JavaConverters._

/**
  * @author Joacim Zschimmer
  */
final class PgpSigner(pgpSecretKey: PGPSecretKey, password: SecretString)
{
  registerBouncyCastle()
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

  override def toString = show"PgpSigner($pgpSecretKey)"
}

object PgpSigner
{
  private val OurHashAlgorithm = HashAlgorithmTags.SHA512

  def writeSecretKeyAsAscii(secretKey: PGPSecretKey, out: OutputStream): Unit = {
    val armored = new ArmoredOutputStream(out)
    new PGPSecretKeyRing(List(secretKey).asJava).encode(armored)
    armored.close()
  }

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

