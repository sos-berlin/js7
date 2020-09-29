package js7.core.crypt.x509

import cats.instances.vector._
import cats.syntax.traverse._
import java.security.cert.{CertificateFactory, X509Certificate}
import java.security.spec.X509EncodedKeySpec
import java.security.{KeyFactory, PublicKey, Signature}
import js7.base.Problems.MessageSignedByUnknownProblem
import js7.base.auth.DistinguishedName
import js7.base.crypt.{GenericSignature, SignatureVerifier, SignerId, X509Signature}
import js7.base.data.ByteArray
import js7.base.problem.{Checked, Problem}
import js7.core.crypt.x509.X509SignatureVerifier.{PublicKeyPem, PublicKeyProvider}
import org.jetbrains.annotations.TestOnly

final class X509SignatureVerifier(publicKeyProviders: Seq[PublicKeyProvider], val publicKeyOrigin: String)
extends SignatureVerifier
{
  protected type MySignature = X509Signature

  def companion = X509SignatureVerifier

  @TestOnly
  def publicKeys = for (o <- publicKeyProviders) yield
    PublicKeyPem.toPem(ByteArray.unsafeWrap(o.publicKey.getEncoded))

  def publicKeysToString =
    s"X.509 origin=$publicKeyOrigin" + publicKeyProviders.map(_.signerId.string).mkString(", ")

  def verify(document: ByteArray, signature: X509Signature): Checked[Seq[SignerId]] =
    Checked.catchNonFatal {
      publicKeyProviders
        .find(o => tryVerify(document, signature, o.publicKey))
        .toRight(MessageSignedByUnknownProblem)
        .map(_.signerId :: Nil)
    }.flatten

  private def tryVerify(document: ByteArray, signature: X509Signature, publicKey: PublicKey): Boolean = {
    val sig = Signature.getInstance(signature.algorithm.string)
    sig.initVerify(publicKey)
    sig.update(document.unsafeArray)
    sig.verify(signature.byteArray.unsafeArray)
  }
}

object X509SignatureVerifier extends SignatureVerifier.Companion
{
  protected type MySignature = X509Signature
  protected type MySignatureVerifier = X509SignatureVerifier

  val typeName = X509Signature.TypeName
  val filenameExtension = ".pem"
  val recommendedKeyDirectoryName = "trusted-x509-keys"

  private val PublicKeyPem = Pem("PUBLIC KEY")
  private val CertificatePem = Pem("CERTIFICATE")

  def checked(publicKeys: Seq[ByteArray], origin: String): Checked[X509SignatureVerifier] =
    publicKeys.toVector.traverse(publicKey =>
      Checked.catchNonFatal {
        val pemString = publicKey.utf8String
        for {
          pemType <- Pem.pemTypeOf(pemString)
          keyAndCert <- pemType match {
            case PublicKeyPem.typeName =>
              PublicKeyPem.fromPem(pemString)
                .map(publicKeyBytes =>
                  KeyFactory.getInstance("RSA")
                    .generatePublic(new X509EncodedKeySpec(publicKeyBytes.unsafeArray)))
                .map(PublicKeyOnly)

            case CertificatePem.typeName =>
              CertificatePem.fromPem(pemString).map(certBytes =>
                CertificateFactory.getInstance("X.509")
                  .generateCertificate(certBytes.toInputStream)
                  .asInstanceOf[X509Certificate])
                .map(Certificate)

            case o => Left(
              Problem.pure(s"For X.509 signature verification, a public key or a certificate is required (not: $o)"))
          }
        } yield keyAndCert
      }.flatten
    ).map(keyAndCerts => new X509SignatureVerifier(keyAndCerts, origin))

  def genericSignatureToSignature(signature: GenericSignature): Checked[X509Signature] =
    X509Signature.fromGenericSignature(signature)

  private[x509] sealed trait PublicKeyProvider {
    def publicKey: PublicKey
    def signerId: SignerId
    def toByteArray: ByteArray
  }

  private[x509] final case class PublicKeyOnly(publicKey: PublicKey) extends PublicKeyProvider {
    lazy val signerId = SignerId("X.509 " + publicKey.toString.takeWhile(_ != '\n'))

    def toByteArray = ByteArray(PublicKeyPem.toPem(ByteArray.unsafeWrap(publicKey.getEncoded)))
  }

  private[x509] final case class Certificate(x509Certificate: X509Certificate)
  extends PublicKeyProvider {
    val publicKey = x509Certificate.getPublicKey
    lazy val signerId = SignerId(new DistinguishedName(x509Certificate.getSubjectX500Principal).string)

    def toByteArray = ByteArray(CertificatePem.toPem(ByteArray.unsafeWrap(x509Certificate.getEncoded)))
  }
}
