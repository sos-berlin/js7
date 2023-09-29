package js7.base.crypt

import io.circe.{Codec, Decoder, Encoder}
import js7.base.annotation.javaApi

/**
  * @author Joacim Zschimmer
  */
final case class GenericSignature(
  typeName: String,
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
    if signatureString.length <= 33 then
      signatureString
    else
      signatureString.take(15) + "..." + signatureString.substring(signatureString.length - 15)
}

object GenericSignature
{
  @javaApi
  def of(typeName: String, signatureString: String) =
    new GenericSignature(typeName, signatureString)

  implicit val jsonCodec: Codec.AsObject[GenericSignature] = {
    Codec.AsObject.from[GenericSignature](
      Decoder.forProduct5(
        "TYPE",
        "signatureString",
        "algorithm",
        "signerId",
        "signerCertificate"
      )(GenericSignature.apply),
      Encoder.forProduct5(
        "TYPE",
        "signatureString",
        "algorithm",
        "signerId",
        "signerCertificate"
      )((o: GenericSignature) => (o.typeName, o.signatureString, o.algorithm, o.signerId, o.signerCertificate)))
  }
}
