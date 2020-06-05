package js7.base.crypt

import js7.base.circeutils.CirceUtils.deriveCodec
import js7.base.utils.Strings._

/**
  * @author Joacim Zschimmer
  */
final case class SignedString(string: String, signature: GenericSignature)
{
  override def toString = s"SignedString(${string.truncateWithEllipsis(100, showLength = true)}, $signature)"
}

object SignedString
{
  implicit val jsonCodec = deriveCodec[SignedString]
}
