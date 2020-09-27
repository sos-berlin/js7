package js7.base.crypt

import java.util.Base64
import js7.base.crypt.X509Signature._
import js7.base.data.ByteArray

final case class X509Signature(byteArray: ByteArray, algorithm: X509Algorithm = defaultAlgorithm)
extends Signature
{
  def toGenericSignature =
    GenericSignature(TypeName, Base64.getMimeEncoder.encodeToString(byteArray.unsafeArray))

  override def toString =
    s"X509Signature(${toGenericSignature.toRawString})"
}

object X509Signature
{
  val TypeName = "X509"
  val defaultAlgorithm = X509Algorithm("SHA512withRSA")

  def fromGenericSignature(signature: GenericSignature) =
    ByteArray.fromMimeBase64(signature.signatureString)
      .map(new X509Signature(_))
}
