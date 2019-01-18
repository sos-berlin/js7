package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.utils.Strings.RichString
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class SignedRepoObject(message: String, signatureType: String, signature: String)
{
  override def toString = s"SignedRepoObject(${message.truncateWithEllipsis(100, showLength = true)}, " +
    s"$signatureType, " +
    s"${signature.truncateWithEllipsis(29, showLength = true)})"
}
