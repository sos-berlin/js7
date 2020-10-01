package js7.core.crypt.x509

import cats.Show
import cats.instances.vector._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import java.security.cert.X509Certificate
import java.security.{PublicKey, Signature, SignatureException}
import js7.base.Problems.{MessageSignedByUnknownProblem, TamperedWithSignedMessageProblem}
import js7.base.crypt.{GenericSignature, SignatureVerifier, SignerId}
import js7.base.data.ByteArray
import js7.base.problem.Checked
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections.implicits._
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.common.scalautil.Logger
import js7.core.crypt.x509.X509.{CertificatePem, pemToCertificate}
import js7.core.crypt.x509.X509SignatureVerifier.logger
import org.jetbrains.annotations.TestOnly
import scala.util.control.NonFatal

final class X509SignatureVerifier private[x509](trustedCertificates: Seq[X509Cert], val publicKeyOrigin: String)
extends SignatureVerifier
{
  protected type MySignature = X509Signature

  def companion = X509SignatureVerifier

  @TestOnly
  def publicKeys = for (o <- trustedCertificates) yield
    CertificatePem.toPem(ByteArray.unsafeWrap(o.x509Certificate.getEncoded))

  def publicKeysToString =
    s"X.509 origin=$publicKeyOrigin " +
      trustedCertificates.map(_.x509Certificate.getSubjectX500Principal.toString).mkString(", ")

  def verify(document: ByteArray, signature: X509Signature): Checked[Seq[SignerId]] =
    Checked.catchNonFatal {
      // We have to try each of the installed trusted certificates !!!
      trustedCertificates.iterator
        .map(tryVerify(document, signature, _))
        .takeWhileInclusive(_.isLeft)
        .toVector
        .lastOption match {
          case None => Left(MessageSignedByUnknownProblem)
          case Some(checkedSignerId) => checkedSignerId.map(_ :: Nil)
        }
    }.flatten

  private def tryVerify(document: ByteArray, signature: X509Signature, trustedCertificate: X509Cert): Checked[SignerId] =
    signature.maybeSignerCertificate match {
      case Some(signerCertificate) =>
        if (!trustedCertificate.isCA)
          Left(MessageSignedByUnknownProblem)
        else
          verifySignersCertificate(signerCertificate, trustedCertificate.x509Certificate.getPublicKey) >>
            verifySignature(document, signature, signerCertificate)

      case _ =>
        verifySignature(document, signature, trustedCertificate)
    }

  private def verifySignature(document: ByteArray, signature: X509Signature, cert: X509Cert): Checked[SignerId] = {
    val sig = Signature.getInstance(signature.algorithm.string)
    sig.initVerify(cert.x509Certificate.getPublicKey)
    sig.update(document.unsafeArray)
    val verified = sig.verify(signature.byteArray.unsafeArray)
    if (!verified) Left(TamperedWithSignedMessageProblem)
    else Right(SignerId(cert.x509Certificate.getSubjectX500Principal.toString))
  }

  private def verifySignersCertificate(signatureCertificate: X509Cert, publicKey: PublicKey): Checked[Unit] =
    try {
      signatureCertificate.x509Certificate.verify(publicKey)
      Right(())
    } catch { case NonFatal(t) =>
      t match {
        case _: SignatureException => logger.debug(t.toStringWithCauses)
        case _ => logger.warn(t.toString)
      }
      Left(MessageSignedByUnknownProblem)
    }
}

object X509SignatureVerifier extends SignatureVerifier.Companion
{
  protected type MySignature = X509Signature
  protected type MySignatureVerifier = X509SignatureVerifier

  val typeName = X509Signature.TypeName
  val filenameExtension = ".pem"
  val recommendedKeyDirectoryName = "trusted-x509-keys"

  private val logger = Logger(getClass)

  implicit val x509CertificateShow: Show[X509Certificate] =
    _.getIssuerX500Principal.toString

  def checked(publicKeys: Seq[ByteArray], origin: String): Checked[X509SignatureVerifier] =
    publicKeys.toVector
      .traverse(publicKey => pemToCertificate(publicKey.utf8String))
      .map(keyAndCerts => new X509SignatureVerifier(keyAndCerts, origin))

  def genericSignatureToSignature(signature: GenericSignature): Checked[X509Signature] = {
    assertThat(signature.typeName == typeName)
    X509Signature.fromGenericSignature(signature)
  }
}
