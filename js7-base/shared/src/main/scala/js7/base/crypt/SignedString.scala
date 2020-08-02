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
  def of(string: String, signatureTypeName: String, signatureString: String) =
    SignedString(string, GenericSignature(signatureTypeName, signatureString))

  implicit val jsonCodec = deriveCodec[SignedString]
}
