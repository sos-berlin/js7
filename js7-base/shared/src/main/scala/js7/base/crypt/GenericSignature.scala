package js7.base.crypt

import io.circe.Codec
import io.circe.generic.extras.JsonKey
import io.circe.generic.extras.defaults.defaultGenericConfiguration
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
import js7.base.annotation.javaApi
import js7.base.utils.IntelliJUtils.intelliJuseImport

/**
  * @author Joacim Zschimmer
  */
final case class GenericSignature(
  @JsonKey("TYPE") typeName: String,
  signatureString: String,
  algorithm: Option[String] = None,
  signerId: Option[SignerId] = None,
  /** Public key, yet to be verified against a root certificate. */
  signerCertificate: Option[String] = None)
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

  implicit val jsonCodec: Codec.AsObject[GenericSignature] = deriveConfiguredCodec

  intelliJuseImport(defaultGenericConfiguration)
}
