package js7.base.crypt

import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.utils.ScalaUtils.syntax._
import org.jetbrains.annotations.TestOnly

/**
  * @author Joacim Zschimmer
  */
final case class SignedString(string: String, signature: GenericSignature)
{
  override def toString = s"SignedString(${string.truncateWithEllipsis(100, showLength = true)}, $signature)"

  /** Tamper in a JSON-compatible way. */
  @TestOnly
  def tamper = copy(string = string + " ")
}

object SignedString
{
  @javaApi
  def of(string: String, signatureTypeName: String, signatureString: String): SignedString =
    SignedString(string, GenericSignature(signatureTypeName, signatureString))

  @javaApi
  def pgp(string: String, signatureString: String): SignedString =
    SignedString(string, GenericSignature("PGP", signatureString))

  @Deprecated
  @javaApi
  def x509WithSignedId(string: String, signatureBase64: String, algorithm: String, signerId: SignerId)
  : SignedString =
    x509WithSignerId(string, signatureBase64, algorithm, signerId)

  @javaApi
  def x509WithSignerId(string: String, signatureBase64: String, algorithm: String, signerId: SignerId)
  : SignedString =
    SignedString(
      string,
      GenericSignature("X509", signatureBase64, algorithm = Some(algorithm), signerId = Some(signerId)))

  @javaApi
  def x509WithCertificate(string: String, base64: String, algorithm: String, signerCertificate: String)
  : SignedString =
    SignedString(
      string,
      GenericSignature("X509", base64, algorithm = Some(algorithm),
        signerCertificate = Some(signerCertificate)))

  implicit val jsonCodec = deriveCodec[SignedString]
}
