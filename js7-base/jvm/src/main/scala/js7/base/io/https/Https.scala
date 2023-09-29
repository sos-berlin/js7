package js7.base.io.https

import java.io.InputStream
import java.security.KeyStore
import java.security.cert.{Certificate, CertificateFactory, X509Certificate}
import javax.net.ssl.{KeyManager, KeyManagerFactory, SSLContext, TrustManagerFactory, X509ExtendedKeyManager, X509KeyManager, X509TrustManager}
import js7.base.crypt.x509.X509Cert
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.generic.SecretString
import js7.base.log.Logger
import js7.base.utils.AutoClosing.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/**
  * Provides HTTPS keystore and truststore..
  * <p>
  * An alternative to an own keystore might have been via Java system properties:
  * <ul>
  *   <li>Server: javax.net.ssl.keyStore=keystore javax.net.ssl.keyStorePassword=password
  *   <li>Client: javax.net.ssl.trustStore=truststore javax.net.ssl.trustStorePassword=trustword
  * </ul>
  *
  * @author Joacim Zschimmer
  * @see https://docs.oracle.com/javase/8/docs/technotes/guides/security/jsse/JSSERefGuide.html#CreateKeystore
  *      https://doc.akka.io/docs/akka-http/current/server-side/server-https-support.html
  *      https://tools.ietf.org/html/rfc5246
  */
object Https
{
  private val logger = Logger[this.type]
  private val PemHeader = ByteArray("-----BEGIN CERTIFICATE-----")
  private val algorithm = KeyManagerFactory.getDefaultAlgorithm  // "SunX509", but for IBM Java: "IbmX509"

  logger.debug(s"algorithm=$algorithm")

  def loadSSLContext(
    keyStoreRef: Option[KeyStoreRef] = None,
    trustStoreRefs: Seq[TrustStoreRef] = Nil)
  : SSLContext = {
    val keyManagers = keyStoreRef.fold(Array.empty[KeyManager])(keyStoreRefToKeyManagers)
    val trustManagers = trustStoreRefToKeyManagers(trustStoreRefs)
    val sslContext = SSLContext.getInstance("TLS")
    sslContext.init(
      keyManagers,
      Array(CompositeX509TrustManager(trustManagers)),
      null)
    sslContext
  }

  private def keyStoreRefToKeyManagers(ref: KeyStoreRef): Array[KeyManager] = {
    val keyStore = loadKeyStore(ref, "private")
    for a <- ref.alias do if !keyStore.containsAlias(a) then throw new IllegalArgumentException(
      s"Unknown alias=$a for $ref - known aliases: ${keyStore.aliases.asScala.mkString(", ")}")
    val factory = KeyManagerFactory.getInstance(algorithm)
    ref.keyPassword.provideCharArray(
      factory.init(keyStore, _))
    val keyManagers = factory.getKeyManagers

    ref.alias.fold(keyManagers)(a =>
      keyManagers.map {
        case km: X509ExtendedKeyManager =>
          new OneAliasX509ExtendedX509KeyManager(km, a)

        case km: X509KeyManager =>
          new OneAliasX509KeyManager
          {
            val keyManager = km
            val alias = a
          }
        case o => o
      })
  }

  private def trustStoreRefToKeyManagers(trustStoreRefs: Seq[TrustStoreRef]): Seq[X509TrustManager] =
    trustStoreRefs
      .flatMap { trustStoreRef =>
        val factory = TrustManagerFactory.getInstance(algorithm)
        factory.init(loadKeyStore(trustStoreRef, "trust"))
        factory.getTrustManagers
      }
      .collect {
        case o: X509TrustManager => Some(o)
        case o =>
          logger.debug(s"Ignoring unknown TrustManager: ${o.getClass.getName} $o")
          None
      }
      .flatten

  private def loadKeyStore(storeRef: StoreRef, kind: String): KeyStore =
    autoClosing(storeRef.url.openStream())(
      loadKeyStoreFromInputStream(_, storeRef.storePassword, storeRef.url.toString, kind))

  private[https] def loadKeyStoreFromInputStream(
    in: InputStream,
    password: SecretString,
    sourcePath: String,
    kind: String)
  : KeyStore = {
    val sizeLimit = 10_000_000
    val keyStore =
      try {
        val content = ByteArray.fromInputStreamLimited(in, sizeLimit)
          .getOrElse(throw new RuntimeException(
            s"Certificate store must not have more than $sizeLimit bytes: $sourcePath"))
        if content startsWith PemHeader then
          pemToKeyStore(content.toInputStream, name = sourcePathToName(sourcePath))
        else
          pkcs12ToKeyStore(content.toInputStream, password)
      } catch { case NonFatal(t) =>
        throw new RuntimeException(s"Cannot load keystore '$sourcePath': $t", t)
      }
    log(keyStore, sourcePath, kind)
    keyStore
  }

  private def pkcs12ToKeyStore(in: InputStream, password: SecretString): KeyStore = {
    val keyStore = KeyStore.getInstance("PKCS12")
    password.provideCharArray(keyStore.load(in, _))
    keyStore
  }

  private def certificateToString(cert: Certificate): String =
    (cert match {
      case cert: X509Certificate =>
        X509Cert(cert).toLongString
      case o =>
        o.getType
    })

  private[https] def sourcePathToName(sourcePath: String): String =
    sourcePath.replace('\\', '/').lastIndexOf('/') match {
      case -1 => sourcePath
      case i => sourcePath.drop(i + 1)
    }

  private def pemToKeyStore(in: InputStream, name: String): KeyStore = {
    val certs = mutable.Buffer.empty[Certificate]
    var eof = false
    while !eof do {
      certs += CertificateFactory.getInstance("X.509").generateCertificate(in)
      in.mark(1)
      eof = in.read() < 0
      if !eof then in.reset()
    }
    val keyStore = KeyStore.getInstance(KeyStore.getDefaultType)
    keyStore.load(null, null)
    for (cert, i) <- certs.zipWithIndex do {
      keyStore.setCertificateEntry(name + (certs.length > 1) ?? s"#${i + 1}", cert)
    }
    keyStore
  }

  private def log(keyStore: KeyStore, sourcePath: String, kind: String): Unit = {
    val iterator = keyStore.aliases.asScala
      .flatMap(a => Option(keyStore.getCertificate(a)).map(a -> _))
    if iterator.isEmpty then {
      logger.warn(s"Loaded empty $kind keystore $sourcePath")
    } else {
      logger.info(s"Loaded $kind keystore $sourcePath" +
        iterator.map { case (alias, cert) =>
          "\n  " +
            (keyStore.isKeyEntry(alias) ?? "Private key ") +
            (keyStore.isCertificateEntry(alias) ?? "Trusted ") +
            certificateToString(cert) +
            " alias=" + alias +
            " (hashCode=" + cert.hashCode + ")"
        }.mkString(""))
    }
  }
}
