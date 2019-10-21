package com.sos.jobscheduler.common.akkahttp.https

import akka.http.scaladsl.{ConnectionContext, HttpsConnectionContext}
import com.sos.jobscheduler.common.akkahttp.https.Https.loadSSLContext

/**
  * @author Joacim Zschimmer
  */
object AkkaHttps
{
  def loadHttpsConnectionContext(keyStoreRef: Option[KeyStoreRef] = None, trustStoreRef: Option[TrustStoreRef] = None): HttpsConnectionContext =
    ConnectionContext.https(
      loadSSLContext(keyStoreRef, trustStoreRef),
      sslConfig = None,
      enabledCipherSuites = None,
      enabledProtocols = None,
      clientAuth = None,
      sslParameters = None)
}
