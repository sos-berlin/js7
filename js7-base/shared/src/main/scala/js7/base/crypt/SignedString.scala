package js7.base.crypt

import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.utils.ScalaUtils.syntax._

/**
  * @author Joacim Zschimmer
  */
final case class SignedString(string: String, signature: GenericSignature)
{
  override def toString = s"SignedString(${string.truncateWithEllipsis(100, showLength = true)}, $signature)"
}

object SignedString
{
  @javaApi
  def of(string: String, signatureTypeName: String, signatureString: String): SignedString =
    SignedString(string, GenericSignature(signatureTypeName, signatureString))

  @javaApi
  def x509WithSignedId(string: String, signatureString: String, algorithm: String, signerId: SignerId): SignedString =
    SignedString(
      string,
      GenericSignature("X509", signatureString, algorithm = Some(algorithm), signerId = Some(signerId)))

  @javaApi
  def x509WithCertificate(string: String, signatureString: String, algorithm: String, signerCertificate: String): SignedString =
    SignedString(
      string,
      GenericSignature("X509", signatureString, algorithm = Some(algorithm), signerCertificate = Some(signerCertificate)))

  implicit val jsonCodec = deriveCodec[SignedString]
}
