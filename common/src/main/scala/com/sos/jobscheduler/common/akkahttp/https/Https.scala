package com.sos.jobscheduler.common.akkahttp.https

import akka.http.scaladsl.{ConnectionContext, HttpsConnectionContext}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.scalautil.AutoClosing._
import com.sos.jobscheduler.common.scalautil.Logger
import java.net.URL
import java.security.cert.{Certificate, X509Certificate}
import java.security.{KeyStore, SecureRandom}
import javax.net.ssl.{KeyManager, KeyManagerFactory, SSLContext, TrustManagerFactory}
import scala.collection.JavaConverters._

/**
  * Provides TLS encryption to Spray HTTP.
  * <p>
  * Another way to use an own keystore may be via Java system properties:
  * <ul>
  *   <li>Server: javax.net.ssl.keyStore=keystore javax.net.ssl.keyStorePassword=password
  *   <li>Client: javax.net.ssl.trustStore=truststore javax.net.ssl.trustStorePassword=trustword
  * </ul>
  *
  * @author Joacim Zschimmer
  * @see http://docs.oracle.com/javase/8/docs/technotes/guides/security/jsse/JSSERefGuide.html#CreateKeystore
  *      https://doc.akka.io/docs/akka-http/current/server-side/server-https-support.html
  *      https://tools.ietf.org/html/rfc5246
  */
object Https
{
  private val logger = Logger(getClass)
  private lazy val KeyManagerAlgorithm = "SunX509"  // Or use default provided by Java ??? {
  //  val r = KeyManagerFactory.getDefaultAlgorithm
  //  logger.debug(s"KeyManagerFactory.getDefaultAlgorithm=$r")
  //  r
  //}

  private lazy val TrustManagerAlgorithm = "SunX509" // Or use default provided by Java ??? {
  //  val r = TrustManagerFactory.getDefaultAlgorithm
  //  logger.debug(s"TrustManagerFactory.getDefaultAlgorithm=$r")
  //  r
  //}

  def toHttpsConnectionContext(keyStoreRef: KeyStoreRef): HttpsConnectionContext =
    ConnectionContext.https(newSSLContext(keyStoreRef))

  def newSSLContext(keyStoreRef: KeyStoreRef): SSLContext =
    toSSLContext(loadKeyStore(keyStoreRef), keyStoreRef.keyPassword)

  private def toSSLContext(keyStore: KeyStore, keyPassword: Option[SecretString]): SSLContext = {
    val keyManagers = keyPassword match {
      case Some(password) ⇒
        val keyManagerFactory = KeyManagerFactory.getInstance(KeyManagerAlgorithm)
        keyManagerFactory.init(keyStore, password.string.toCharArray)
        keyManagerFactory.getKeyManagers
      case _ ⇒
        Array.empty[KeyManager]
    }

    val trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerAlgorithm)
    trustManagerFactory.init(keyStore)

    val sslContext = SSLContext.getInstance("TLS")
    val trustManagers = trustManagerFactory.getTrustManagers
    sslContext.init(keyManagers, trustManagers, new SecureRandom)
    sslContext
  }

  private[https] def loadKeyStore(keyStoreRef: KeyStoreRef): KeyStore = {
    val keyStore = KeyStore.getInstance("PKCS12")
    autoClosing(keyStoreRef.url.openStream()) { inputStream ⇒
      keyStore.load(inputStream, keyStoreRef.storePassword.string.toCharArray)
    }
    log(keyStoreRef.url, keyStore)
    keyStore
  }

  private def log(url: URL, keyStore: KeyStore): Unit =
    logger.info(s"Loaded key store $url: ${keyStoreToString(keyStore)}")
  //{
  //  val aliases = keyStore.aliases.asScala
  //  if (aliases.isEmpty)
  //    logger.info("Key store does not contain any certificate")
  //  else
  //    for (alias ← aliases) {
  //      logger.info(certificateToString(keyStore.getCertificate(alias)) +
  //        (if (keyStore.isKeyEntry(alias)) " (private key)" else "")) +
  //        ", alias=" + alias
  //    }
  //}

  def keyStoreToString(keyStore: KeyStore): String = {
    val aliases = keyStore.aliases.asScala
    if (aliases.isEmpty)
      "Key store does not contain any certificate"
    else
      aliases.map(alias ⇒
        s"Alias $alias: " +
        certificateToString(keyStore.getCertificate(alias)) +
          (if (keyStore.isKeyEntry(alias)) " (private key)" else ""))
      .mkString(", ")
  }

  private def certificateToString(cert: Certificate): String =
    cert match {
      case cert: X509Certificate ⇒
        "X509 certificate " +
          '"' + cert.getSubjectX500Principal.toString + '"'
          //", valid from " + Timestamp.ofEpochMilli(cert.getNotBefore.getTime) +
          //" until " + Timestamp.ofEpochMilli(cert.getNotAfter.getTime)
      case o ⇒
        o.getClass.getName
    }
}
