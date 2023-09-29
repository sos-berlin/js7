package js7.base.crypt.x509

import java.security.MessageDigest
import java.security.cert.{CertificateFactory, X509Certificate}
import js7.base.auth.{DistinguishedName, Pem}
import js7.base.crypt.SignerId
import js7.base.crypt.x509.X509Cert.*
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import scala.jdk.CollectionConverters.*

private[x509] final case class X509Cert(x509Certificate: X509Certificate):
  import x509Certificate.*

  lazy val signersDistinguishedName = new DistinguishedName(getSubjectX500Principal)
  lazy val signerId = SignerId(signersDistinguishedName.toString)

  lazy val fingerprint: ByteArray =
    val md = MessageDigest.getInstance("SHA-1")
    md.update(getEncoded)
    ByteArray(md.digest)

  lazy val isCA =
    containsCA(getCriticalExtensionOIDs) ||
      containsCA(getNonCriticalExtensionOIDs)

  private def containsCA(strings: java.util.Set[String]) =
    Option(strings).fold(false)(_.contains(MayActAsCA))

  def toLongString =
    "X.509 certificate " +
      getSubjectX500Principal + " · " +
      (isCA ?? "CA, ") +
      "fingerprint=" + fingerprint.toHexRaw +
      (getKeyUsage != null) ?? (" keyUsage=" + keyUsageToString(getKeyUsage)) +
      (getExtendedKeyUsage != null) ??
        (" extendedKeyUsage=" +
          getExtendedKeyUsage.asScala.map(o => oidToString.getOrElse(o, o)).mkString(",")) +
      ((getSubjectAlternativeNames != null) ??
        (" subjectAlternativeNames=" + subjectAlternativeNamesToString(
          getSubjectAlternativeNames)))

  override def toString =
    s"X.509Certificate($getSubjectX500Principal)"

object X509Cert:
  private val MayActAsCA = "2.5.29.19"
  val CertificatePem = Pem("CERTIFICATE")
  val PrivateKeyPem = Pem("PRIVATE KEY")

  def fromPem(pem: String): Checked[X509Cert] =
    CertificatePem.fromPem(pem) flatMap fromByteArray

  def fromByteArray(byteArray: ByteArray): Checked[X509Cert] =
    Checked.catchNonFatal:
      val certificate = CertificateFactory.getInstance("X.509")
        .generateCertificate(byteArray.toInputStream)
        .asInstanceOf[X509Certificate]
      certificate.checkValidity()  // throws
      X509Cert(certificate)

  private val keyUsages = Vector(
    "digitalSignature",
    "nonRepudiation",
    "keyEncipherment",
    "dataEncipherment",
    "keyAgreement",
    "keyCertSign",
    "crlSign",
    "encipherOnly",
    "decipherOnly")

  private def keyUsageToString(keyUsage: Array[Boolean]): String =
    keyUsages.indices.flatMap(i => keyUsage(i) ? keyUsages(i)).mkString(",")

  private val oidToString = Map[String, String](
    "1.3.6.1.5.5.7.3.1" -> "serverAuth",
    "1.3.6.1.5.5.7.3.2" -> "clientAuth")

  private def subjectAlternativeNamesToString(collection: java.util.Collection[java.util.List[?]])
  : String =
    collection.asScala.map(_.asScala.toArray[Any] match {
      case Array(i, value) =>
        (i match {
          case i: java.lang.Integer => subjectAlternativeKeys.getOrElse(i, i.toString)
          case i => i.toString
        }) + "=" + (value match {
          case value: String => value
          case _ => "..."
        })
    }).mkString(",")

  private val subjectAlternativeKeys = Map(
    0 -> "other",
    1 -> "rfc822",
    2 -> "DNS",
    3 -> "x400Address",
    4 -> "directory",
    5 -> "ediParty",
    6 -> "uniformResourceIdentifier",
    7 -> "IP",
    8 -> "registeredID")
