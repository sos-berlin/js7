package js7.base.crypt.x509

import java.util.Base64
import js7.base.crypt.x509.X509Cert.CertificatePem
import js7.base.crypt.x509.X509Signature.*
import js7.base.crypt.{GenericSignature, Signature, SignerId}
import js7.base.data.ByteArray
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp

private[x509] final case class X509Signature(
  byteArray: ByteArray,
  algorithm: X509Algorithm,
  signerIdOrCertificate: SignerId | X509Cert)
extends Signature:

  def toGenericSignature =
    val (signerId, cert) = signerIdOrCertificate match
      case o: SignerId => Some(o) -> None
      case o: X509Cert => None -> Some(o)
    GenericSignature(
      TypeName,
      Base64.getMimeEncoder.encodeToString(byteArray.unsafeArray),
      algorithm = Some(algorithm.string),
      signerId = signerId,
      signerCertificate = cert.map: o =>
        CertificatePem.toPem(ByteArray(o.x509Certificate.getEncoded)))

  override def toString =
    "X509Signature( " +
      toGenericSignature.toRawString + ", " +
      signerIdOrCertificate.match
        case o: SignerId => o.string
        case o: X509Cert => ByteArray(o.x509Certificate.getEncoded)
    + ")"


object X509Signature:
  val TypeName = "X509"

  def fromGenericSignature(signature: GenericSignature, checkExpiry: Option[Timestamp])
  : Checked[X509Signature] =
    for
      signatureBytes <- ByteArray.fromMimeBase64(signature.signatureString)
      algorithm <- signature.algorithm match
        case None => Left(Problem.pure("Missing X.509 signature algorithm"))
        case Some(o) => Right(X509Algorithm(o))
      signerIdOrCertificate <-
        (signature.signerId, signature.signerCertificate) match
          case (Some(signerId), None) => Right(signerId)
          case (None, Some(cert)) => X509Cert.fromPem(cert, checkExpiry)
          case _ =>
            Left(Problem.pure("X.509 signature requires either a signerId or a signerCertificate"))
    yield
      X509Signature(signatureBytes, algorithm, signerIdOrCertificate)
