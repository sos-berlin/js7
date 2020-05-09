package com.sos.jobscheduler.base.crypt

import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
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

  override def toString = s"Signature(" +
    (if (signatureString.length <= 33) signatureString
     else signatureString.take(15) + "..." + signatureString.substring(signatureString.length - 15)) +
    ")"
}

object GenericSignature {
  intelliJuseImport(defaultGenericConfiguration)
}
