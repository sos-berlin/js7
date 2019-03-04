package com.sos.jobscheduler.data.crypt

import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.Strings.RichString
import io.circe.generic.extras.defaults.defaultGenericConfiguration
import io.circe.generic.extras.{ConfiguredJsonCodec, JsonKey}

/**
  * @author Joacim Zschimmer
  */
@ConfiguredJsonCodec
final case class GenericSignature(
  @JsonKey("TYPE") typeName: String,
  signatureString: String)
extends Signature
{
  def toGenericSignature = this

  override def toString = s"Signature(${signatureString.truncateWithEllipsis(20)})"
}

object GenericSignature {
  intelliJuseImport(defaultGenericConfiguration)
}
