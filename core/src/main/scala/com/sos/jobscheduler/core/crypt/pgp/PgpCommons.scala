package com.sos.jobscheduler.core.crypt.pgp

import cats.Show
import cats.effect.{Resource, SyncIO}
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.show._
import com.sos.jobscheduler.base.time.Timestamp.JavaUtilDateShow
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.SyncResource.ops._
import java.io.{InputStream, OutputStream}
import java.security.Security
import org.bouncycastle.bcpg.{ArmoredOutputStream, HashAlgorithmTags}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openpgp.examples.PubringDump
import org.bouncycastle.openpgp.operator.KeyFingerPrintCalculator
import org.bouncycastle.openpgp.operator.jcajce.JcaKeyFingerprintCalculator
import org.bouncycastle.openpgp.{PGPPublicKey, PGPPublicKeyRing, PGPPublicKeyRingCollection, PGPSecretKey, PGPSecretKeyRing, PGPSecretKeyRingCollection, PGPSignature, PGPUtil}
import scala.collection.JavaConverters._
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object PgpCommons
{
  Security.addProvider(new BouncyCastleProvider)

  private val BufferSize = 4096

  implicit val PGPPublicKeyShow = Show[PGPPublicKey] { key ⇒
    import key._
    f"PGPPublicKey($getKeyID%08X " +
      getCreationTime.show + " " +
      getUserIDs.asScala.mkString("'", "', '", "'") + " " +
      "algorithm=" + publicKeyAlgorithmToString(getAlgorithm) + " " +
      "isEncryptionKey=" + isEncryptionKey + " " +
      "isMasterKey=" + isMasterKey + " " +
      "fingerprint=" + fingerprintToString(getFingerprint) +
      ")"
  }

  implicit val PGPPublicKeyRingShow = Show[PGPPublicKeyRing](
    _.asScala.toVector.mkString_("PGPPublicKeyRing(", ", ", ")"))

  implicit val PGPPublicKeyRingCollectionShow = Show[PGPPublicKeyRingCollection](
    _.asScala.toVector.mkString_("", ", ", ""))

  implicit val PGPSecretKeyShow = Show[PGPSecretKey] { key ⇒
    import key._
    f"PGPSecretKey($getKeyID%08X " +
      getPublicKey.show + " " +
      "cipher=" + cipherToString(getKeyEncryptionAlgorithm) + " " +
      "isSigningKey=" + isSigningKey + " " +
      "isMasterKey=" + isMasterKey + " " +
      ")"
  }

  implicit val PGPSecretKeyRingShow = Show[PGPSecretKeyRing](o ⇒
    "PGPSecretKeyRing(" + o.getPublicKey.show + ")")

  implicit val PGPSecretKeyRingCollectionShow = Show[PGPSecretKeyRingCollection](o ⇒
    f"PGPSecretKeyRingCollection(${o.asScala.toVector.mkString_("", ", ", "")})")

  implicit val PGPSignatureShow = Show[PGPSignature] { sig ⇒
    import sig._
    f"PGPSignature($getKeyID%08X " +
      "hash=" + hashAlgorithToString(getHashAlgorithm) + " " +
      "keyAlgorithm=" + publicKeyAlgorithmToString(getKeyAlgorithm) + " " +
      getCreationTime.show +
      ")"
  }

  private def hashAlgorithToString(hashAlgorithm: Int) =
    hashAlgorithm match {
      case HashAlgorithmTags.SHA1 ⇒ "SHA-1"
      case HashAlgorithmTags.MD2 ⇒ "MD2"
      case HashAlgorithmTags.MD5 ⇒ "MD5"
      case HashAlgorithmTags.RIPEMD160 ⇒ "RIPEMD160"
      case HashAlgorithmTags.SHA256 ⇒ "SHA-256"
      case HashAlgorithmTags.SHA384 ⇒ "SHA-384"
      case HashAlgorithmTags.SHA512 ⇒ "SHA-512"
      case HashAlgorithmTags.SHA224 ⇒ "SHA-224"
      case HashAlgorithmTags.TIGER_192 ⇒ "TIGER"
      case _ ⇒ hashAlgorithm.toString
    }

  private def publicKeyAlgorithmToString(n: Int) =
    try PubringDump.getAlgorithm(n)
    catch { case NonFatal(_) ⇒ s"digest-$n" }

  private def cipherToString(n: Int) =
    try PGPUtil.getSymmetricCipherName(n)
    catch { case NonFatal(_) ⇒ s"cipher-$n" }

  private def fingerprintToString(fingerprint: Array[Byte]): String =
    fingerprint match {
      case null ⇒ "(no fingerprint)"
      case bytes ⇒ bytes.map(b ⇒ f"$b%02X").mkString
    }

  private[crypt] def registerBouncyCastle() = ()  // Dummy to initialize this object

  private[crypt] def readMessage(message: Resource[SyncIO, InputStream], update: (Array[Byte], Int) ⇒ Unit): Unit =
    message.useSync { in ⇒
      val buffer = new Array[Byte](BufferSize)
      var length = 1
      while (length > 0) {
        length = in.read(buffer)
        if (length > 0) {
          update(buffer, length)
        }
      }
    }

  def writePublicKeyAsAscii(publicKey: PGPPublicKey, out: OutputStream): Unit = {
    val armored = new ArmoredOutputStream(out)
    publicKey.encode(armored)
    armored.close()
  }

  def readPublicKeyRingCollection(resource: Resource[SyncIO, InputStream]): PGPPublicKeyRingCollection =
    resource.useSync(in ⇒
      new PGPPublicKeyRingCollection(PGPUtil.getDecoderStream(in), newFingerPrintCalculator))

  def newFingerPrintCalculator: KeyFingerPrintCalculator =
    new JcaKeyFingerprintCalculator  // or BcKeyFingerprintCalculator

  def toPublicKeyRingCollection(publicKey: PGPPublicKey): PGPPublicKeyRingCollection = {
    val ring = new PGPPublicKeyRing((publicKey :: Nil).asJava)
    new PGPPublicKeyRingCollection((ring :: Nil).asJava)
  }

  intelliJuseImport(JavaUtilDateShow)
}
