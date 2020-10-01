package js7.core.crypt.x509

import java.security.cert.{CertificateFactory, X509Certificate}
import js7.base.auth.Pem
import js7.base.data.ByteArray
import js7.base.problem.Checked

private[x509] object X509
{
  val CertificatePem = Pem("CERTIFICATE")

  def pemToCertificate(pem: String): Checked[X509Cert] =
    CertificatePem.fromPem(pem).flatMap(bytesToCertificate)

  def bytesToCertificate(byteArray: ByteArray): Checked[X509Cert] =
    Checked.catchNonFatal {
      val certificate = CertificateFactory.getInstance("X.509")
        .generateCertificate(byteArray.toInputStream)
        .asInstanceOf[X509Certificate]
      certificate.checkValidity()  // throws
      X509Cert(certificate)
    }
}
