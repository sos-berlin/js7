package js7.service.pgp

import cats.Show
import cats.effect.{Resource, SyncIO}
import cats.instances.vector.*
import cats.syntax.foldable.*
import cats.syntax.show.*
import cats.syntax.traverse.*
import java.io.{ByteArrayOutputStream, InputStream, OutputStream}
import java.nio.charset.StandardCharsets.US_ASCII
import java.security.Security
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTime.*
import js7.base.utils.Labeled
import js7.base.utils.SyncResource.syntax.*
import org.bouncycastle.bcpg.{ArmoredOutputStream, HashAlgorithmTags, PublicKeyAlgorithmTags}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openpgp.examples.PubringDump
import org.bouncycastle.openpgp.operator.KeyFingerPrintCalculator
import org.bouncycastle.openpgp.operator.jcajce.JcaKeyFingerprintCalculator
import org.bouncycastle.openpgp.{PGPPublicKey, PGPPublicKeyRing, PGPPublicKeyRingCollection, PGPSecretKey, PGPSecretKeyRing, PGPSecretKeyRingCollection, PGPSignature, PGPUtil}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object PgpCommons
{
  private val logger = Logger[this.type]

  Security.addProvider(new BouncyCastleProvider)

  private val BufferSize = 4096

  def pgpPublicKeyToShortString(key: PGPPublicKey) =
    "PGPPublicKey" +
      " userIDs=" + key.getUserIDs.asScala.mkString("'", "', '", "'") +
      " fingerprint=" + fingerPrintAsString(key)

  implicit val PGPPublicKeyShow: Show[PGPPublicKey] =
    Show[PGPPublicKey] { key =>
      import key.*
      f"PGPPublicKey($getKeyID%08X" +
        " userIDs=" + getUserIDs.asScala.mkString("'", "', '", "'") +
        " fingerprint=" + fingerPrintAsString(key) +
        " created=" + getCreationTime.show +
        " algorithm=" + publicKeyAlgorithmToString(getAlgorithm) +
        " isEncryptionKey=" + isEncryptionKey +
        " isMasterKey=" + isMasterKey +
        ")"
    }

  private def fingerPrintAsString(key: PGPPublicKey): String =
    Option(key.getFingerprint).fold(ByteArray.empty)(ByteArray(_)).toHexRaw

  implicit val PGPPublicKeyRingShow: Show[PGPPublicKeyRing] =
    Show(_.asScala.toVector.mkString_("PGPPublicKeyRing(", ", ", ")"))

  implicit val PGPPublicKeyRingCollectionShow: Show[PGPPublicKeyRingCollection] =
    Show[PGPPublicKeyRingCollection](_
      .asScala.toVector.mkString_("", ", ", ""))

  implicit val PGPSecretKeyShow: Show[PGPSecretKey] =
    Show { key =>
      import key.*
      "PGPSecretKey(" +
        getPublicKey.show +
        " cipher=" + cipherToString(getKeyEncryptionAlgorithm) +
        " isSigningKey=" + isSigningKey +
        " isMasterKey=" + isMasterKey +
        ")"
    }

  implicit val PGPSecretKeyRingShow: Show[PGPSecretKeyRing] =
    Show(o =>
      "PGPSecretKeyRing(" + o.getPublicKey.show + ")")

  implicit val PGPSecretKeyRingCollectionShow: Show[PGPSecretKeyRingCollection] =
    Show(o =>
      f"PGPSecretKeyRingCollection(${o.asScala.toVector.mkString_("", ", ", "")})")

  implicit val PGPSignatureShow: Show[PGPSignature] =
    Show { sig =>
      import sig.*
      "PGPSignature(" +
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

  private[pgp] def registerBouncyCastle() = ()  // Dummy to initialize this object

  private def readMessage(message: Resource[SyncIO, InputStream], update: (Array[Byte], Int) => Unit): Unit =
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

  def writePublicKeyRingCollectionAsAscii(publicKey: PGPPublicKeyRingCollection, out: OutputStream)
  : Unit = {
    val armored = new ArmoredOutputStream(out)
    publicKey.encode(armored)
    armored.close()
  }

  def readPublicKeyRingCollection(keys: Seq[Labeled[ByteArray]])
  : Checked[PGPPublicKeyRingCollection] =
    keys.traverse(toPGPPublicKeyRing)
      .map(keyRings =>
        new PGPPublicKeyRingCollection(keyRings.asJava))

  def readOrIgnorePublicKeyRingCollection(keys: Seq[Labeled[ByteArray]])
  : PGPPublicKeyRingCollection =
    new PGPPublicKeyRingCollection(
      keys
        .flatMap(key =>
          toPGPPublicKeyRing(key) match {
            case Left(problem) =>
              logger.error(s"Ignoring PGP public key due to: $problem")
              None
            case Right(o) => Some(o)
          })
        .asJava)

  private def toPGPPublicKeyRing(labeledKey: Labeled[ByteArray]): Checked[PGPPublicKeyRing] =
    try
      Right(
        new PGPPublicKeyRing(
          PGPUtil.getDecoderStream(labeledKey.value.toInputStream),
            newFingerPrintCalculator()))
    catch {
      case NonFatal(t) => Left(Problem.fromThrowable(t).withKey(labeledKey.label))
    }

  def newFingerPrintCalculator(): KeyFingerPrintCalculator =
    new JcaKeyFingerprintCalculator  // or BcKeyFingerprintCalculator

  def toPublicKeyRingCollection(publicKey: PGPPublicKey): PGPPublicKeyRingCollection = {
    val ring = new PGPPublicKeyRing((publicKey :: Nil).asJava)
    new PGPPublicKeyRingCollection((ring :: Nil).asJava)
  }

  implicit final class RichPGPPublicKey(private val underlying: PGPPublicKey) extends AnyVal {
    def toArmoredAsciiBytes: ByteArray = {
      val out = new ByteArrayOutputStream()
      writePublicKeyAsAscii(underlying, out)
      ByteArray.unsafeWrap(out.toByteArray)
    }
  }

  implicit final class RichPGPPublicKeyRingCollection(
    private val underlying: PGPPublicKeyRingCollection)
  extends AnyVal {
    def toArmoredString: String = {
      val out = new ByteArrayOutputStream()
      writePublicKeyRingCollectionAsAscii(underlying, out)
      new String(out.toByteArray, US_ASCII)
    }
  }
}
