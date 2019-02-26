package com.sos.jobscheduler.core.crypt.pgp

import cats.Show
import cats.effect.{Resource, SyncIO}
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.show._
import com.sos.jobscheduler.base.time.Timestamp.JavaUtilDateShow
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.SyncResource.ops._
import java.io.{ByteArrayOutputStream, InputStream, OutputStream}
import java.security.Security
import org.bouncycastle.bcpg.{ArmoredOutputStream, HashAlgorithmTags, PublicKeyAlgorithmTags}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openpgp.examples.PubringDump
import org.bouncycastle.openpgp.operator.KeyFingerPrintCalculator
import org.bouncycastle.openpgp.operator.jcajce.JcaKeyFingerprintCalculator
import org.bouncycastle.openpgp.{PGPPublicKey, PGPPublicKeyRing, PGPPublicKeyRingCollection, PGPSecretKey, PGPSecretKeyRing, PGPSecretKeyRingCollection, PGPSignature, PGPUtil}
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object PgpCommons
{
  Security.addProvider(new BouncyCastleProvider)

  private val BufferSize = 4096

  def pgpPublicKeyToShortString(key: PGPPublicKey) = {
    import key._
    f"PGPPublicKey(" +
      "fingerprint=" + fingerprintToString(getFingerprint) +
      " userIDs=" + getUserIDs.asScala.mkString("'", "', '", "'") +
      ")"
  }

  implicit val PGPPublicKeyShow = Show[PGPPublicKey] { key =>
    import key._
    f"PGPPublicKey($getKeyID%08X" +
      " fingerprint=" + fingerprintToString(getFingerprint) +
      " userIDs=" + getUserIDs.asScala.mkString("'", "', '", "'") +
      " created=" + getCreationTime.show +
      " algorithm=" + publicKeyAlgorithmToString(getAlgorithm) +
      " isEncryptionKey=" + isEncryptionKey +
      " isMasterKey=" + isMasterKey +
      ")"
  }

  implicit val PGPPublicKeyRingShow = Show[PGPPublicKeyRing](
    _.asScala.toVector.mkString_("PGPPublicKeyRing(", ", ", ")"))

  implicit val PGPPublicKeyRingCollectionShow = Show[PGPPublicKeyRingCollection](
    _.asScala.toVector.mkString_("", ", ", ""))

  implicit val PGPSecretKeyShow = Show[PGPSecretKey] { key =>
    import key._
    f"PGPSecretKey(" +
      getPublicKey.show +
      " cipher=" + cipherToString(getKeyEncryptionAlgorithm) +
      " isSigningKey=" + isSigningKey +
      " isMasterKey=" + isMasterKey +
      ")"
  }

  implicit val PGPSecretKeyRingShow = Show[PGPSecretKeyRing](o =>
    "PGPSecretKeyRing(" + o.getPublicKey.show + ")")

  implicit val PGPSecretKeyRingCollectionShow = Show[PGPSecretKeyRingCollection](o =>
    f"PGPSecretKeyRingCollection(${o.asScala.toVector.mkString_("", ", ", "")})")

  implicit val PGPSignatureShow = Show[PGPSignature] { sig =>
    import sig._
    f"PGPSignature(" +
      signatureTypeToString(getSignatureType) +
      //PGPUtil.getSignatureName(getKeyAlgorithm, getHashAlgorithm)
      ", created=" + getCreationTime.show +
      " hash=" + hashAlgorithmToString(getHashAlgorithm) +
      //" keyAlgorithm=" + publicKeyAlgorithmToString(getKeyAlgorithm) +
      f" publicKeyID=$getKeyID%08X" +
      ")"
  }

  private def signatureTypeToString(t: Int) = t match {
    case PGPSignature.BINARY_DOCUMENT          => "binary document"
    case PGPSignature.CANONICAL_TEXT_DOCUMENT  => "canonical text document"
    case PGPSignature.STAND_ALONE              => "stand alone"
    case PGPSignature.DEFAULT_CERTIFICATION    => "default certification"
    case PGPSignature.NO_CERTIFICATION         => "no certification"
    case PGPSignature.CASUAL_CERTIFICATION     => "casual certification"
    case PGPSignature.POSITIVE_CERTIFICATION   => "positive certification"
    case PGPSignature.SUBKEY_BINDING           => "subkey binding"
    case PGPSignature.PRIMARYKEY_BINDING       => "primarykey binding"
    case PGPSignature.DIRECT_KEY               => "direct key"
    case PGPSignature.KEY_REVOCATION           => "key revocation"
    case PGPSignature.SUBKEY_REVOCATION        => "subkey revocation"
    case PGPSignature.CERTIFICATION_REVOCATION => "certification revocation"
    case PGPSignature.TIMESTAMP                => "timestamp"
    case _ => t.toString
  }

  private def hashAlgorithmToString(hashAlgorithm: Int) =
    hashAlgorithm match {
      case HashAlgorithmTags.SHA1 => "SHA-1"
      case HashAlgorithmTags.MD2 => "MD2"
      case HashAlgorithmTags.MD5 => "MD5"
      case HashAlgorithmTags.RIPEMD160 => "RIPEMD160"
      case HashAlgorithmTags.SHA256 => "SHA-256"
      case HashAlgorithmTags.SHA384 => "SHA-384"
      case HashAlgorithmTags.SHA512 => "SHA-512"
      case HashAlgorithmTags.SHA224 => "SHA-224"
      case HashAlgorithmTags.TIGER_192 => "TIGER"
      case _ => hashAlgorithm.toString
    }

  private def publicKeyAlgorithmToString(n: Int) =
    n match {
      case PublicKeyAlgorithmTags.RSA_GENERAL => "'RSA general'"
      case PublicKeyAlgorithmTags.RSA_ENCRYPT => "'RSA encrypt'"
      case PublicKeyAlgorithmTags.RSA_SIGN => "'RSA sign'"
      case PublicKeyAlgorithmTags.ELGAMAL_ENCRYPT => "'El Gamal encrypt'"
      case PublicKeyAlgorithmTags.ELGAMAL_GENERAL => "'El Gamal general'"
      case PublicKeyAlgorithmTags.DIFFIE_HELLMAN => "Diffie-Hellman"
      case _ =>
        try PubringDump.getAlgorithm(n)
        catch { case NonFatal(_) => n.toString }
      }

  private def cipherToString(n: Int) =
    try PGPUtil.getSymmetricCipherName(n)
    catch { case NonFatal(_) => s"cipher-$n" }

  private def fingerprintToString(fingerprint: Array[Byte]): String =
    fingerprint match {
      case null => "(no fingerprint)"
      case bytes => bytes.map(b => f"$b%02X").grouped(2).map(_.mkString).mkString(" ")
    }

  private[crypt] def registerBouncyCastle() = ()  // Dummy to initialize this object

  private[crypt] def readMessage(message: Resource[SyncIO, InputStream], update: (Array[Byte], Int) => Unit): Unit =
    message.useSync { in =>
      val buffer = new Array[Byte](BufferSize)
      var length = 1
      while (length > 0) {
        length = in.read(buffer)
        if (length > 0) {
          update(buffer, length)
        }
      }
    }

  def writeSecretKeyAsAscii(secretKey: PGPSecretKey, out: OutputStream): Unit = {
    val armored = new ArmoredOutputStream(out)
    new PGPSecretKeyRing(List(secretKey).asJava).encode(armored)
    armored.close()
  }

  def writePublicKeyAsAscii(publicKey: PGPPublicKey, out: OutputStream): Unit = {
    val armored = new ArmoredOutputStream(out)
    publicKey.encode(armored)
    armored.close()
  }

  def writePublicKeyRingCollectionAsAscii(publicKey: PGPPublicKeyRingCollection, out: OutputStream): Unit = {
    val armored = new ArmoredOutputStream(out)
    publicKey.encode(armored)
    armored.close()
  }

  def readPublicKeyRingCollection(resource: Resource[SyncIO, InputStream]): PGPPublicKeyRingCollection =
    resource.useSync(in =>
      new PGPPublicKeyRingCollection(PGPUtil.getDecoderStream(in), newFingerPrintCalculator))

  def newFingerPrintCalculator: KeyFingerPrintCalculator =
    new JcaKeyFingerprintCalculator  // or BcKeyFingerprintCalculator

  def toPublicKeyRingCollection(publicKey: PGPPublicKey): PGPPublicKeyRingCollection = {
    val ring = new PGPPublicKeyRing((publicKey :: Nil).asJava)
    new PGPPublicKeyRingCollection((ring :: Nil).asJava)
  }

  implicit final class RichPGPSecretKey(private val underlying: PGPSecretKey) extends AnyVal {
    def toArmoredAsciiBytes: Seq[Byte] = {
      val out = new ByteArrayOutputStream()
      writeSecretKeyAsAscii(underlying, out)
      out.toByteArray.toVector
    }
  }

  implicit final class RichPGPPublicKey(private val underlying: PGPPublicKey) extends AnyVal {
    def toArmoredAsciiBytes: Seq[Byte] = {
      val out = new ByteArrayOutputStream()
      writePublicKeyAsAscii(underlying, out)
      out.toByteArray.toVector
    }
  }

  implicit final class RichPGPPublicKeyRingCollection(private val underlying: PGPPublicKeyRingCollection) extends AnyVal {
    def toArmoredAsciiBytes: Seq[Byte] = {
      val out = new ByteArrayOutputStream()
      writePublicKeyRingCollectionAsAscii(underlying, out)
      out.toByteArray.toVector
    }
  }

  intelliJuseImport(JavaUtilDateShow)
}
