package com.sos.jobscheduler.master.client

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.common.akkahttp.https.{Https, KeyStoreRef}
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser

/**
  * @author Joacim Zschimmer
  */
final class AkkaHttpMasterApi(
  protected val baseUri: Uri,
  keyStoreRef: Option[KeyStoreRef] = None)
extends HttpMasterApi
with AkkaHttpClient
with ProvideActorSystem
{
  protected val baseUriString = baseUri.toString

  override protected lazy val httpsConnectionContextOption =
    keyStoreRef map Https.toHttpsConnectionContext

  protected def httpClient = this

  closer.onClose { super[AkkaHttpClient].close() }

  override def close() = super[ProvideActorSystem].closer.close()
}
