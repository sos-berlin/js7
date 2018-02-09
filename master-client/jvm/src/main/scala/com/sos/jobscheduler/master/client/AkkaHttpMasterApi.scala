package com.sos.jobscheduler.master.client

import akka.actor.ActorSystem

/**
  * @author Joacim Zschimmer
  */
final class AkkaHttpMasterApi(val uri: String) extends AutoCloseable with HttpMasterApi with AkkaHttpClient {
  protected def httpClient = this
  protected val actorSystem = ActorSystem("AkkaHttpClient")
  protected val executionContext = actorSystem.dispatcher

  override def close() =
    try super.close()
    finally actorSystem.terminate()
}
