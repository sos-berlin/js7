package js7.base.crypt

import java.util.Optional
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.utils.ScalaUtils.syntax._
import scala.jdk.OptionConverters._

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
    of(string, signatureTypeName, signatureString, Optional.empty)

  @javaApi
  def of(string: String, signatureTypeName: String, signatureString: String, signerCertificate: Optional[String]): SignedString =
    SignedString(string, GenericSignature(signatureTypeName, signatureString, signerCertificate.toScala))

  implicit val jsonCodec = deriveCodec[SignedString]
}
