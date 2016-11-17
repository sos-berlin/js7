package com.sos.scheduler.engine.common.sprayutils.https

import akka.actor.ActorSystem
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.base.utils.ScalaUtils.cast
import com.sos.scheduler.engine.common.scalautil.AutoClosing._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import java.security.KeyStore
import java.util.concurrent.atomic.AtomicBoolean
import javax.net.ssl.{KeyManager, KeyManagerFactory, SSLContext, SSLEngine, TrustManagerFactory}
import scala.collection.JavaConversions._
import spray.can.Http.HostConnectorSetup
import spray.http.Uri
import spray.io.{ClientSSLEngineProvider, ServerSSLEngineProvider}

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
  private val SslContextAlgorithm = "TLS"
  private val logger = Logger(getClass)
  private val alreadyLogged = new AtomicBoolean
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

  def toHostConnectorSetup(keystore: KeystoreReference, uri: Uri)(implicit actorSystem: ActorSystem): HostConnectorSetup =
    toHostConnectorSetup(keystore, cast[Uri.NonEmptyHost](uri.authority.host), uri.effectivePort)

  def toHostConnectorSetup(keystore: KeystoreReference, host: Uri.NonEmptyHost, port: Int)(implicit actorSystem: ActorSystem): HostConnectorSetup =
    HostConnectorSetup(host = host.address, port = port, sslEncryption = true)(actorSystem, newClientSSLEngineProvider(keystore))

  def newClientSSLEngineProvider(keystore: KeystoreReference): ClientSSLEngineProvider = {
    implicit val sslContext = newSSLContext(keystore)
    ClientSSLEngineProvider { sslEngine ⇒
      //logger.debug(s"SSLEngine peer=${sslEngine.getPeerHost}:${sslEngine.getPeerPort}")
      sslEngine
    }
  }

  def newServerSSLEngineProvider(keystoreRef: KeystoreReference): ServerSSLEngineProvider = {
    implicit def sslContext: SSLContext = newSSLContext(keystoreRef)
    ServerSSLEngineProvider(enableThingsInSSLEngine)
  }

  private def enableThingsInSSLEngine(engine: SSLEngine): SSLEngine = {
//    !!! When used by ClientSSLEngineProvider, exception here let Spray endlessly retry the HTTP request
//    try engine.setEnabledCipherSuites(Array("TLS_RSA_WITH_AES_256_CBC_SHA"))  // Needs "Java Cryptography Extension (JCE) Unlimited Strength Jurisdiction Policy Files 8"
//    catch {
//      case NonFatal(t) ⇒
//        logger.warn(s"$t")
//        logger.info(s"SSLEngine#getEnabledCipherSuites (ordered) = ${engine.getEnabledCipherSuites.sorted mkString " "}")
//        logger.info(s"SSLEngine#getSupportedCipherSuites (ordered) = ${engine.getSupportedCipherSuites.sorted mkString " "}")
//        throw t
//    }
    val logged = alreadyLogged getAndSet true
    if (!logged) logger.info(s"SSLEngine#getEnabledCipherSuites (ordered) = ${engine.getEnabledCipherSuites.sorted mkString " "}")
    engine.setEnabledProtocols(Array("TLSv1.2"))
    if (!logged) logger.info(s"SSLEngine#getEnabledProtocols (ordered) = ${engine.getEnabledProtocols.sorted mkString " "}")
    engine
  }

  def newSSLContext(keystoreRef: KeystoreReference): SSLContext =
    newSSLContext(loadKeyStore(keystoreRef), keyPassword = keystoreRef.keyPassword)


  def newSSLContext(keystore: KeyStore, keyPassword: Option[SecretString]): SSLContext = {
    val keyManagers = keyPassword match {
      case Some(password) ⇒
        val keyManagerFactory = KeyManagerFactory.getInstance(KeyManagerAlgorithm)
        keyManagerFactory.init(keystore, password.string.toArray)
        keyManagerFactory.getKeyManagers
      case None ⇒ Array[KeyManager]()
    }
    val trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerAlgorithm) sideEffect {
      _.init(keystore)
    }
    SSLContext.getInstance(SslContextAlgorithm) sideEffect {
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
