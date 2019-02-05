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
  string: String)
extends Signature
{
  override def toString = s"PgpSignature(${string.truncateWithEllipsis(29, showLength = true)})"
}

object GenericSignature {
  intelliJuseImport(defaultGenericConfiguration)
}
