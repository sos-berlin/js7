package js7.common.crypt.x509

import java.security.cert.{CertificateFactory, X509Certificate}
import js7.base.auth.{DistinguishedName, Pem}
import js7.base.crypt.SignerId
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops._
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import js7.common.crypt.x509.X509Cert._
import scala.jdk.CollectionConverters._

private[x509] final case class X509Cert(x509Certificate: X509Certificate)
{
  lazy val isCA = Option(x509Certificate.getCriticalExtensionOIDs.asScala)
    .getOrElse(Set.empty[String])
    .contains(MayActAsCA)

  lazy val signersDistinguishedName = new DistinguishedName(x509Certificate.getSubjectX500Principal)
  lazy val signerId = SignerId(signersDistinguishedName.toString)

  override def toString = s"X.509Certificate(${x509Certificate.getSubjectX500Principal} " +
    (isCA ?? "CA ") +
    s"serialNr=${x509Certificate.getSerialNumber} modulus=${ByteArray(x509Certificate.getEncoded).toHexRaw(16)})"
}

object X509Cert
{
  private val MayActAsCA = "2.5.29.19"
  val CertificatePem = Pem("CERTIFICATE")

  def fromPem(pem: String): Checked[X509Cert] =
    CertificatePem.fromPem(pem) flatMap fromByteArray

  def fromByteArray(byteArray: ByteArray): Checked[X509Cert] =
    Checked.catchNonFatal {
      val certificate = CertificateFactory.getInstance("X.509")
        .generateCertificate(byteArray.toInputStream)
        .asInstanceOf[X509Certificate]
      certificate.checkValidity()  // throws
      X509Cert(certificate)
    }
}
