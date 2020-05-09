package com.sos.jobscheduler.base.crypt

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCodec
import com.sos.jobscheduler.base.utils.Strings._

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
