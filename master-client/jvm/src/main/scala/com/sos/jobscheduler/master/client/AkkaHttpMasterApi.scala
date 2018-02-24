package com.sos.jobscheduler.master.client

import com.sos.jobscheduler.common.akkautils.ProvideActorSystem
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersCloser

/**
  * @author Joacim Zschimmer
  */
final class AkkaHttpMasterApi(val uri: String) extends HttpMasterApi with AkkaHttpClient with ProvideActorSystem
{
  protected def httpClient = this
  protected def executionContext = actorSystem.dispatcher
  protected def userAndPassword = None

  closer.onClose { super.close() }

  override def close() = closer.close()
}
