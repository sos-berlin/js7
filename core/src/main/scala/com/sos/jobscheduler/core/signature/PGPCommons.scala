package com.sos.jobscheduler.core.signature

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
import org.bouncycastle.bcpg.ArmoredOutputStream
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openpgp.examples.PubringDump
import org.bouncycastle.openpgp.{PGPPublicKey, PGPPublicKeyRing, PGPPublicKeyRingCollection, PGPSecretKey, PGPSecretKeyRing, PGPSecretKeyRingCollection, PGPUtil}
import scala.collection.JavaConverters._
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object PGPCommons
{
  Security.addProvider(new BouncyCastleProvider)

  private val BufferSize = 4096

  implicit val PGPPublicKeyShow = Show[PGPPublicKey](o ⇒
    f"PGPPublicKey(${o.getKeyID}%08X " +
      show"${o.getCreationTime} " +
      o.getUserIDs.asScala.mkString("'", "', '", "'") + " " +
      "algorithm=" + publicKeyAlgorithmToString(o.getAlgorithm) + " " +
      s"isEncryptionKey=${o.isEncryptionKey} " +
      s"isMasterKey=${o.isMasterKey} " +
      "fingerprint=" + fingerprintToString(o.getFingerprint) +
      ")")

  implicit val PGPPublicKeyRingShow = Show[PGPPublicKeyRing](
    _.asScala.toVector.mkString_("PGPPublicKeyRing(", ", ", ")"))

  implicit val PGPPublicKeyRingCollectionShow = Show[PGPPublicKeyRingCollection](
    _.asScala.toVector.mkString_("", ", ", ""))

  implicit val PGPSecretKeyShow = Show[PGPSecretKey](o ⇒
    f"PGPSecretKey(${o.getKeyID}%08X " +
      show"${o.getPublicKey} " +
      "cipher=" + cipherToString(o.getKeyEncryptionAlgorithm) + " " +
      s"isSigningKey=${o.isSigningKey} " +
      s"isMasterKey=${o.isMasterKey} " +
      ")")

  implicit val PGPSecretKeyRingShow = Show[PGPSecretKeyRing](o ⇒
    show"PGPSecretKeyRing(" +
      show"${o.getPublicKey} " +
      ")")

  implicit val PGPSecretKeyRingCollectionShow = Show[PGPSecretKeyRingCollection](o ⇒
    f"PGPSecretKeyRingCollection(${o.asScala.toVector.mkString_("", ", ", "")})")

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

  private[signature] def registerBountyCastle() = ()  // Dummy to initialize this object

  private[signature] def readMessage(message: Resource[SyncIO, InputStream], update: (Array[Byte], Int) ⇒ Unit): Unit =
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

  def writePublicKeyAscii(publicKey: PGPPublicKey, out: OutputStream): Unit = {
    val armored = new ArmoredOutputStream(out)
    publicKey.encode(armored)
    armored.close()
  }

  intelliJuseImport(JavaUtilDateShow)
}
