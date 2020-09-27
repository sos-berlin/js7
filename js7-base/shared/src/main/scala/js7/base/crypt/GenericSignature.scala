package js7.base.crypt

import io.circe.generic.extras.defaults.defaultGenericConfiguration
import io.circe.generic.extras.{ConfiguredJsonCodec, JsonKey}
import js7.base.annotation.javaApi
import js7.base.utils.IntelliJUtils.intelliJuseImport

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

  override def toString = s"Signature($toRawString)"

  def toRawString =
    if (signatureString.length <= 33)
      signatureString
    else
      signatureString.take(15) + "..." + signatureString.substring(signatureString.length - 15)
}

object GenericSignature
{
  @javaApi
  def of(typeName: String, signatureString: String) =
    new GenericSignature(typeName, signatureString)

  intelliJuseImport(defaultGenericConfiguration)
}
