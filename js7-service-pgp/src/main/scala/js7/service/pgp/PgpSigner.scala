package js7.service.pgp

import cats.instances.vector.*
import cats.syntax.foldable.*
import cats.syntax.show.*
import java.util.Base64
import js7.base.crypt.pgp.PgpSignature
import js7.base.crypt.{DocumentSigner, SignerId}
import js7.base.data.ByteArray
import js7.base.generic.SecretString
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.utils.Labeled
import js7.service.pgp.PgpCommons.*
import org.bouncycastle.bcpg.HashAlgorithmTags
import org.bouncycastle.openpgp.operator.jcajce.{JcaPGPContentSignerBuilder, JcePBESecretKeyDecryptorBuilder}
import org.bouncycastle.openpgp.{PGPSecretKey, PGPSecretKeyRingCollection, PGPSignature, PGPSignatureGenerator, PGPSignatureSubpacketGenerator, PGPUtil}
import scala.jdk.CollectionConverters.*
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
final class PgpSigner private(pgpSecretKey: PGPSecretKey, password: SecretString)
extends DocumentSigner:
  protected type MySignature = PgpSignature

  def companion = PgpSigner

  import PgpSigner.*

  registerBouncyCastle()
  //logger.debug(pgpSecretKey.show)

  private val pgpPrivateKey = pgpSecretKey.extractPrivateKey(
    new JcePBESecretKeyDecryptorBuilder() //?new JcaPGPDigestCalculatorProviderBuilder()
      .setProvider("BC")
      .build(password.string.toArray))
  private val maybeUserId: Option[String] = pgpSecretKey.getPublicKey.getUserIDs.asScala.buffered.headOption  // Only the first UserID ?

  def sign(message: ByteArray): PgpSignature =
    val signatureGenerator = newSignatureGenerator()
    signatureGenerator.update(message.unsafeArray)
    val signatureBytes = signatureGenerator.generate.getEncoded(/*forTransfer=*/true)
    PgpSignature(Base64.getMimeEncoder.encodeToString(signatureBytes))

  private def newSignatureGenerator(): PGPSignatureGenerator =
    val signatureGenerator = new PGPSignatureGenerator(
      new JcaPGPContentSignerBuilder(pgpSecretKey.getPublicKey.getAlgorithm, OurHashAlgorithm)
        .setProvider("BC"))
    signatureGenerator.init(PGPSignature.BINARY_DOCUMENT, pgpPrivateKey)
    for u <- maybeUserId do
      val subpacketGenerator = new PGPSignatureSubpacketGenerator
      subpacketGenerator.addSignerUserID(false, u)
      signatureGenerator.setHashedSubpackets(subpacketGenerator.generate())
    signatureGenerator

  override def toString = show"PgpSigner($pgpSecretKey)"


object PgpSigner extends DocumentSigner.Companion:
  protected type MySignature = PgpSignature
  protected type MyMessageSigner = PgpSigner

  def typeName = PgpSignature.TypeName

  def checked(privateKey: ByteArray, password: SecretString) =
    Checked.catchNonFatal(
      new PgpSigner(selectSecretKey(readSecretKeyRingCollection(privateKey)), password))

  private val OurHashAlgorithm = HashAlgorithmTags.SHA512

  def apply(pgpSecretKey: PGPSecretKey, password: SecretString): Checked[PgpSigner] =
    Checked.catchNonFatal(
      new PgpSigner(pgpSecretKey, password))

  def forTest() =
    val pgpPassword = SecretString(Vector.fill(10)('a' + Random.nextInt('z' - 'a' + 1)).mkString)
    val pgpSecretKey = PgpKeyGenerator.generateSecretKey(SignerId("TEST"), pgpPassword, keySize = 1024/*fast for test*/)
    PgpSigner(pgpSecretKey, pgpPassword).orThrow ->
      PgpSignatureVerifier
        .checked(
          Seq(Labeled(pgpSecretKey.getPublicKey.toArmoredAsciiBytes, "PgpSigner.forTest")),
          origin = "PgpSigner")
        .orThrow

  private def readSecretKeyRingCollection(byteArray: ByteArray): PGPSecretKeyRingCollection =
    new PGPSecretKeyRingCollection(
      PGPUtil.getDecoderStream(byteArray.toInputStream),
      newFingerPrintCalculator())

  private def selectSecretKey(keyRings: PGPSecretKeyRingCollection): PGPSecretKey =
    val keys = keyRings
      .getKeyRings.asScala
      .map(o => o.getSecretKey(o.getPublicKey/*the controller key*/.getFingerprint))
      .toVector
    if keys.isEmpty then throw new NoSuchElementException("No controller key in secret key ring")
    if keys.sizeIs > 1 then throw new IllegalArgumentException(
      "More than one controller key in secret key ring: " +
        keys.mkString_("", ", ", ""))
    keys.head
