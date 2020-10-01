package js7.core.crypt.x509

import cats.instances.option._
import cats.syntax.traverse._
import java.util.Base64
import js7.base.crypt.{GenericSignature, Signature}
import js7.base.data.ByteArray
import js7.base.problem.Checked
import js7.core.crypt.x509.X509.{CertificatePem, pemToCertificate}
import js7.core.crypt.x509.X509Signature._

private[x509] final case class X509Signature(
  byteArray: ByteArray,
  maybeSignerCertificate: Option[X509Cert] = None,
  algorithm: X509Algorithm = defaultAlgorithm)
extends Signature
{
  def toGenericSignature =
    GenericSignature(
      TypeName,
      Base64.getMimeEncoder.encodeToString(byteArray.unsafeArray),
      maybeSignerCertificate.map(o => CertificatePem.toPem(ByteArray(o.x509Certificate.getEncoded))))

  override def toString =
    "X509Signature( " +
      toGenericSignature.toRawString +
      maybeSignerCertificate.fold("")(o => s",${ByteArray(o.x509Certificate.getEncoded)}") +
      ")"
}

object X509Signature
{
  val TypeName = "X509"
  val defaultAlgorithm = X509Algorithm("SHA512withRSA")

  def fromGenericSignature(signature: GenericSignature): Checked[X509Signature] =
    for {
      signatureBytes <- ByteArray.fromMimeBase64(signature.signatureString)
      maybeSignerCert <- signature.signerCertificate.traverse(pemToCertificate)
    } yield new X509Signature(signatureBytes, maybeSignerCert)
}
