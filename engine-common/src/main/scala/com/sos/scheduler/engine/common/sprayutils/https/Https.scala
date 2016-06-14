package com.sos.scheduler.engine.common.sprayutils.https

import akka.actor.ActorSystem
import akka.io.IO
import com.sos.scheduler.engine.base.generic.SecretString
import com.sos.scheduler.engine.common.scalautil.AutoClosing._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import java.security.KeyStore
import java.util.concurrent.atomic.AtomicBoolean
import javax.net.ssl.{KeyManagerFactory, SSLContext, SSLEngine, TrustManagerFactory}
import scala.collection.JavaConversions._
import spray.can.Http
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

  def acceptTlsCertificateFor(keystoreRef: KeystoreReference, uri: Uri)(implicit actorSystem: ActorSystem): Unit =
    acceptTlsCertificateFor(keystoreRef, uri.authority.host.address, uri.effectivePort)

  def acceptTlsCertificateFor(keystore: KeystoreReference, host: String, port: Int)(implicit actorSystem: ActorSystem): Unit =
    IO(Http) ! {
      implicit val myEngineProvider = newClientSSLEngineProvider(keystore)
      HostConnectorSetup(host = host, port = port, sslEncryption = true)
    }

  def newClientSSLEngineProvider(keystore: KeystoreReference): ClientSSLEngineProvider = {
    implicit lazy val sslContext = newSSLContext(keystore)
    ClientSSLEngineProvider(identity)
  }

  def newServerSSLEngineProvider(keystoreRef: KeystoreReference): ServerSSLEngineProvider = {
    implicit def sslContext: SSLContext = newSSLContext(keystoreRef)
    ServerSSLEngineProvider(enableThingsInSSLEngine)
  }

  private def enableThingsInSSLEngine(engine: SSLEngine): SSLEngine = {
//    //FIXME When used by ClientSSLEngineProvider, exception here let Spray endlessly retry the HTTP request
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

  def newSSLContext(keystore: KeyStore, keyPassword: SecretString): SSLContext = {
    val keyManagerFactory = KeyManagerFactory.getInstance(KeyManagerAlgorithm) sideEffect {
      _.init(keystore, keyPassword.string.toArray)
    }
    val trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerAlgorithm) sideEffect {
      _.init(keystore)
    }
    val r = SSLContext.getInstance(SslContextAlgorithm)
    r.init(keyManagerFactory.getKeyManagers, trustManagerFactory.getTrustManagers, null)
    r
  }

  private def loadKeyStore(keystoreRef: KeystoreReference): KeyStore = {
    val keystore = KeyStore.getInstance(KeyStore.getDefaultType)
    logger.debug(s"Loading $keystoreRef")
    autoClosing(keystoreRef.url.openStream()) { in ⇒
      val pw = (keystoreRef.storePassword map { _.string.toCharArray }).orNull
      keystore.load(in, pw)
    }
    for (alias ← keystore.aliases; cert = keystore.getCertificate(alias)) logger.debug(s"$alias: $cert")
    keystore
  }
}
