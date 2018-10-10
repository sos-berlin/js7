package com.sos.jobscheduler.master.client

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.base.utils.ScalazStyle._
import com.sos.jobscheduler.common.akkahttp.https.AkkaHttps.loadHttpsConnectionContext
import com.sos.jobscheduler.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.http.AkkaHttpClient

/**
  * @author Joacim Zschimmer
  */
final class AkkaHttpMasterApi(
  protected val baseUri: Uri,
  /** To provide a client certificate to server. */
  keyStoreRef: Option[KeyStoreRef] = None,
  /** To trust the server's certificate. */
  trustStoreRef: Option[TrustStoreRef] = None)
extends HttpMasterApi
with AkkaHttpClient
with ProvideActorSystem
{
  protected val baseUriString = baseUri.toString

  override protected lazy val httpsConnectionContextOption =
    (keyStoreRef.nonEmpty || trustStoreRef.nonEmpty) ? loadHttpsConnectionContext(keyStoreRef, trustStoreRef)  // TODO None means HttpsConnectionContext? Or empty context?

  protected def httpClient = this

  closer.onClose { super[AkkaHttpClient].close() }

  override def close() = super[ProvideActorSystem].closer.close()
}
