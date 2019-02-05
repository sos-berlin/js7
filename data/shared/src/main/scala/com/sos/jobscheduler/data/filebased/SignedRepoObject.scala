package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.data.crypt.GenericSignature
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class SignedRepoObject(message: String, signature: GenericSignature)
{
  override def toString = s"SignedRepoObject(${message.truncateWithEllipsis(100, showLength = true)}, $signature)"
}
