package js7.core.crypt.x509

import com.typesafe.scalalogging.Logger
import java.security.cert.X509Certificate
import js7.core.crypt.x509.X509Cert._
import scala.jdk.CollectionConverters._

private[x509] final case class X509Cert(x509Certificate: X509Certificate)
{
  lazy val isCA = Option(x509Certificate.getCriticalExtensionOIDs.asScala)
    .getOrElse(Set.empty[String])
    .contains(MayActAsCA)
}

object X509Cert
{
  private val logger = Logger(getClass)
  private val MayActAsCA = "2.5.29.19"
}
