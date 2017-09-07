package com.sos.jobscheduler.common.akkahttp.https

import akka.http.scaladsl.HttpsConnectionContext
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.common.scalautil.AutoClosing._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.scalautil.SideEffect.ImplicitSideEffect
import java.security.KeyStore
import javax.net.ssl.{KeyManager, KeyManagerFactory, SSLContext, TrustManagerFactory}
import scala.collection.JavaConversions._

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
  *      http://spray.io/documentation/1.2.3/spray-can/http-server/#ssl-support
  *      https://tools.ietf.org/html/rfc5246
  *
  */
object Https {
  private val logger = Logger(getClass)
  private lazy val KeyManagerAlgorithm = {
    val r = KeyManagerFactory.getDefaultAlgorithm
    logger.debug(s"KeyManagerFactory.getDefaultAlgorithm=$r")
    r
  }
  private lazy val TrustManagerAlgorithm = {
    val r = TrustManagerFactory.getDefaultAlgorithm
    logger.debug(s"TrustManagerFactory.getDefaultAlgorithm=$r")
    r
  }

  def toHttpsConnectionContext(keystore: KeystoreReference): HttpsConnectionContext =
    new HttpsConnectionContext(newSSLContext(keystore))

  private def newSSLContext(keystoreRef: KeystoreReference): SSLContext =
    newSSLContext(loadKeyStore(keystoreRef), keyPassword = keystoreRef.keyPassword)

  private def newSSLContext(keystore: KeyStore, keyPassword: Option[SecretString]): SSLContext = {
    val keyManagers = keyPassword match {
      case Some(password) ⇒
        val keyManagerFactory = KeyManagerFactory.getInstance(KeyManagerAlgorithm)
        keyManagerFactory.init(keystore, password.string.toCharArray)
        keyManagerFactory.getKeyManagers
      case None ⇒ Array[KeyManager]()
    }
    val trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerAlgorithm) sideEffect {
      _.init(keystore)
    }
    SSLContext.getInstance("TLS") sideEffect {
      _.init(keyManagers, trustManagerFactory.getTrustManagers, null)
    }
  }

  private def loadKeyStore(keystoreRef: KeystoreReference): KeyStore = {
    val keystore = KeyStore.getInstance(KeyStore.getDefaultType)
    logger.info(s"Loading $keystoreRef")
    autoClosing(keystoreRef.url.openStream()) { in ⇒
      val pw = (keystoreRef.storePassword map { _.string.toCharArray }).orNull
      keystore.load(in, pw)
    }
    for (alias ← keystore.aliases; cert = keystore.getCertificate(alias)) logger.debug(s"Alias $alias: $cert")
    keystore
  }
}
