package js7.common.akkahttp.https

import akka.http.scaladsl.{ConnectionContext, HttpsConnectionContext}
import js7.common.akkahttp.https.Https.loadSSLContext

/**
  * @author Joacim Zschimmer
  */
object AkkaHttps
{
  def loadHttpsConnectionContext(keyStoreRef: Option[KeyStoreRef] = None, trustStoreRefs: Seq[TrustStoreRef] = Nil): HttpsConnectionContext =
    ConnectionContext.https(
      loadSSLContext(keyStoreRef, trustStoreRefs),
      sslConfig = None,
      enabledCipherSuites = None,
      enabledProtocols = None,
      clientAuth = None,
      sslParameters = None)
}
