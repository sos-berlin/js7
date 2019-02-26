package com.sos.jobscheduler.common.akkahttp.https

import com.sos.jobscheduler.common.scalautil.AutoClosing._
import com.sos.jobscheduler.common.scalautil.Logger
import java.net.URL
import java.security.cert.{Certificate, X509Certificate}
import java.security.{KeyStore, SecureRandom}
import javax.net.ssl.{KeyManager, KeyManagerFactory, SSLContext, TrustManager, TrustManagerFactory}
import scala.collection.JavaConverters._

/**
  * Provides HTTPS keystore and truststore..
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
  //private lazy val KeyManagerAlgorithm = {
  //  val r = KeyManagerFactory.getDefaultAlgorithm
  //  logger.debug(s"KeyManagerFactory.getDefaultAlgorithm=$r")
  //  r
  //}
  //private lazy val TrustManagerAlgorithm = {
  //  val r = TrustManagerFactory.getDefaultAlgorithm
  //  logger.debug(s"TrustManagerFactory.getDefaultAlgorithm=$r")
  //  r
  //}

  def loadSSLContext(keyStoreRef: Option[KeyStoreRef] = None, trustStoreRef: Option[TrustStoreRef] = None): SSLContext = {
    val keyManagers = keyStoreRef match {
      case None => Array.empty[KeyManager]
      case Some(ref) =>
        val keyStore = loadKeyStore(ref)
        val factory = KeyManagerFactory.getInstance("SunX509")
        factory.init(keyStore, ref.keyPassword.string.toCharArray)
        factory.getKeyManagers
    }
    val trustManagers = trustStoreRef match {
      case None => Array.empty[TrustManager]
      case Some(ref) =>
        val keyStore = loadKeyStore(ref)
        val factory = TrustManagerFactory.getInstance("SunX509")
        factory.init(keyStore)
        factory.getTrustManagers
    }
    val sslContext = SSLContext.getInstance("TLS")
    sslContext.init(keyManagers, trustManagers, new SecureRandom)
    sslContext
  }

  private def loadKeyStore(storeRef: StoreRef): KeyStore = {
    val keyStore = KeyStore.getInstance("PKCS12")
    autoClosing(storeRef.url.openStream()) { inputStream =>
      keyStore.load(inputStream, storeRef.storePassword.string.toCharArray)
    }
    log(storeRef.url, keyStore)
    keyStore
  }

  private def log(url: URL, keyStore: KeyStore): Unit =
    logger.info(s"Loaded key store $url: ${keyStoreToString(keyStore)}")
  //{
  //  val aliases = keyStore.aliases.asScala
  //  if (aliases.isEmpty)
  //    logger.info("Key store does not contain any certificate")
  //  else
  //    for (alias <- aliases) {
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
      aliases.map(alias =>
        s"Alias $alias: " +
        certificateToString(keyStore.getCertificate(alias)) +
          (if (keyStore.isKeyEntry(alias)) " (private key)" else ""))
      .mkString(", ")
  }

  private def certificateToString(cert: Certificate): String =
    cert match {
      case cert: X509Certificate =>
        "X509 certificate " +
          '"' + cert.getSubjectX500Principal.toString + '"'
          //", valid from " + Timestamp.ofEpochMilli(cert.getNotBefore.getTime) +
          //" until " + Timestamp.ofEpochMilli(cert.getNotAfter.getTime)
      case o =>
        o.getClass.getName
    }
}
