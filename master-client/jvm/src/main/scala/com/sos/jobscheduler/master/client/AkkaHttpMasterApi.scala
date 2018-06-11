package com.sos.jobscheduler.master.client

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser

/**
  * @author Joacim Zschimmer
  */
final class AkkaHttpMasterApi(protected val baseUri: Uri)
extends HttpMasterApi
with AkkaHttpClient
with ProvideActorSystem
{
  protected val baseUriString = baseUri.toString

  protected def httpClient = this

  closer.onClose { super[AkkaHttpClient].close() }

  override def close() = super[ProvideActorSystem].closer.close()
}
