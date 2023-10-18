package js7.base.crypt.x509

import java.util.Base64
import js7.base.crypt.x509.X509Cert.CertificatePem
import js7.base.crypt.x509.X509Signature.*
import js7.base.crypt.{GenericSignature, Signature, SignerId}
import js7.base.data.ByteArray
import js7.base.problem.{Checked, Problem}

private[x509] final case class X509Signature(
  byteArray: ByteArray,
  algorithm: X509Algorithm,
  signerIdOrCertificate: Either[SignerId, X509Cert])
extends Signature:

  def toGenericSignature =
    GenericSignature(
      TypeName,
      Base64.getMimeEncoder.encodeToString(byteArray.unsafeArray),
      algorithm = Some(algorithm.string),
      signerId = signerIdOrCertificate.left.toOption,
      signerCertificate = signerIdOrCertificate.toOption
        .map(o => CertificatePem.toPem(ByteArray(o.x509Certificate.getEncoded))))

  override def toString =
    "X509Signature( " +
      toGenericSignature.toRawString + ", " +
      signerIdOrCertificate.fold(_.string, o => ByteArray(o.x509Certificate.getEncoded)) +
      ")"


object X509Signature:
  val TypeName = "X509"

  def fromGenericSignature(signature: GenericSignature): Checked[X509Signature] =
    for
      signatureBytes <- ByteArray.fromMimeBase64(signature.signatureString)
      algorithm <- signature.algorithm match
        case None => Left(Problem.pure("Missing X.509 signature algorithm"))
        case Some(o) => Right(X509Algorithm(o))
      signerIdOrCertificate <-
        (signature.signerId, signature.signerCertificate) match
          case (Some(signerId), None) => Right(Left(signerId))
          case (None, Some(cert)) => X509Cert.fromPem(cert) map Right.apply
          case _ => Left(Problem.pure("X.509 signature requires either a signerId or a signerCertificate"))
    yield new X509Signature(signatureBytes, algorithm, signerIdOrCertificate)
