package com.sos.jobscheduler.data.crypt

import com.sos.jobscheduler.base.utils.Strings._
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class SignedString(string: String, signature: GenericSignature)
{
  override def toString = s"SignedString(${string.truncateWithEllipsis(100, showLength = true)}, $signature)"
}
