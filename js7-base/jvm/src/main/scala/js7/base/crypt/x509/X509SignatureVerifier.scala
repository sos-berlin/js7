package js7.base.crypt.x509

import cats.Show
import cats.instances.vector.*
import cats.syntax.traverse.*
import java.security.cert.X509Certificate
import java.security.{PublicKey, Signature, SignatureException}
import js7.base.Problems.{MessageSignedByUnknownProblem, TamperedWithSignedMessageProblem}
import js7.base.auth.DistinguishedName
import js7.base.crypt.x509.X509Cert.CertificatePem
import js7.base.crypt.x509.X509SignatureVerifier.logger
import js7.base.crypt.{GenericSignature, SignatureVerifier, SignerId}
import js7.base.data.ByteArray
import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.Collections.duplicatesToProblem
import js7.base.utils.Collections.implicits.*
import js7.base.utils.Labeled
import js7.base.utils.ScalaUtils.syntax.{RichPartialFunction, RichThrowable}
import org.jetbrains.annotations.TestOnly
import scala.util.control.NonFatal

final class X509SignatureVerifier private[x509](
  trustedCertificates: Seq[X509Cert],
  trustedRootCertificates: Seq[X509Cert],
  signerDNToTrustedCertificate: Map[DistinguishedName, X509Cert],
  val publicKeyOrigin: String)
extends SignatureVerifier
{
  protected type MySignature = X509Signature

  def companion = X509SignatureVerifier

  @TestOnly
  def publicKeys = for (o <- trustedCertificates) yield
    CertificatePem.toPem(ByteArray.unsafeWrap(o.x509Certificate.getEncoded))

  def publicKeysToStrings =
    Seq(s"X.509 origin=$publicKeyOrigin") ++
      trustedCertificates
        .map(o => "  " + o.toLongString)

  def verify(document: ByteArray, signature: X509Signature): Checked[Seq[SignerId]] =
    signature.signerIdOrCertificate match {
      case Left(signerId) =>
        DistinguishedName.checked(signerId.string)
          .flatMap(dn =>
            signerDNToTrustedCertificate.rightOr(dn,
              Problem(s"The signature's SignerId is unknown: ${signerId.string}")))
          .flatMap(trustedCertificate =>
            verifySignature(document, signature, trustedCertificate)
              .map(_ :: Nil))

      case Right(signerCertificate) =>
        Checked.catchNonFatal {
          // We have to try each of the installed trusted certificates !!!
          trustedRootCertificates.iterator
            .map(rootCert =>
              for {
                signerId <- verifySignature(document, signature, signerCertificate)
                _ <- verifySignersCertificate(signerCertificate, rootCert.x509Certificate.getPublicKey)
              } yield signerId)
            .takeWhileInclusive(_.isLeft)
            .toVector
            .lastOption match {
              case None =>
                Left(MessageSignedByUnknownProblem)
              case Some(checkedSignerId) =>
                checkedSignerId.map(_ :: Nil)
            }
        }.flatten
    }

  private def verifySignature(document: ByteArray, signature: X509Signature, cert: X509Cert): Checked[SignerId] =
    Checked.catchNonFatal {
      val sig = Signature.getInstance(signature.algorithm.string)
      sig.initVerify(cert.x509Certificate.getPublicKey)
      sig.update(document.unsafeArray)
      val verified = sig.verify(signature.byteArray.unsafeArray)
      if (!verified) Left(TamperedWithSignedMessageProblem)
      else Right(SignerId(cert.x509Certificate.getSubjectX500Principal.toString))
    }.flatten

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

  private val logger = Logger[this.type]
  val typeName = X509Signature.TypeName
  val filenameExtension = ".pem"
  val recommendedKeyDirectoryName = "trusted-x509-keys"

  implicit val x509CertificateShow: Show[X509Certificate] =
    _.getIssuerX500Principal.toString

  def checked(pems: Seq[Labeled[ByteArray]], origin: String): Checked[X509SignatureVerifier] =
    pems.toVector
      .traverse(labeledPem => X509Cert.fromPem(labeledPem.value.utf8String))
      .flatMap(_
        .toCheckedKeyedMap(_.signersDistinguishedName, duplicateDNsToProblem)
        .map(toVerifier(_, origin)))

  def ignoreInvalid(pems: Seq[Labeled[ByteArray]], origin: String): X509SignatureVerifier = {
    val certs = pems.flatMap(labeledPem =>
      X509Cert.fromPem(labeledPem.value.utf8String) match {
        case Left(problem) =>
          logger.error(s"Ignoring X.509 certificate '${labeledPem.label}' due to: $problem")
          None
        case Right(o) => Some(o)
      })
    for (problem <- certs.checkUniqueness(_.signersDistinguishedName).left)
      logger.error(s"Duplicate certificates: $problem")
    toVerifier(certs.toKeyedMap(_.signersDistinguishedName), origin)
  }

  private def toVerifier(signerDNToTrustedCertificate: Map[DistinguishedName, X509Cert], origin: String)
  : X509SignatureVerifier = {
    val trustedCertificates = signerDNToTrustedCertificate.values.toVector
    // Openssl 1.1.1i always sets the CA critical extension
    // to allow self-signed certificates (?)
    //.filterNot(_.isCA)
    val rootCertificates = trustedCertificates.filter(_.isCA)
    for (o <- rootCertificates) logger.debug(
      s"Trusting signatures signed with a certificate which is signed with root $o")
    for (o <- signerDNToTrustedCertificate.values) logger.debug(
      s"Trusting signatures signed with $o")
    new X509SignatureVerifier(
      trustedCertificates,
      rootCertificates,
      signerDNToTrustedCertificate,
      origin)
  }

  private def duplicateDNsToProblem(duplicates: Map[DistinguishedName, Iterable[?]]) =
    duplicatesToProblem("Duplicate X.509 certificates", duplicates)

  def genericSignatureToSignature(signature: GenericSignature): Checked[X509Signature] = {
    assertThat(signature.typeName == typeName)
    X509Signature.fromGenericSignature(signature)
  }
}
