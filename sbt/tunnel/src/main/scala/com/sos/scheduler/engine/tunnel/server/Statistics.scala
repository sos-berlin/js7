package com.sos.scheduler.engine.tunnel.server

import java.time.Instant
import java.time.Instant._
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
private[server] case class Statistics(  // We don't care about synchronization ???
  var requestCount: Int = 0,
  var messageByteCount: Long = 0,
  var currentRequestIssuedAt: Option[Instant] = None,
  var failure: Option[Throwable] = None) {

  def updateWith(request: Connector.Request)(implicit ec: ExecutionContext): Unit = {
    currentRequestIssuedAt = Some(now)
    requestCount += 1
    messageByteCount += request.message.size
    request.responsePromise.future.onComplete { tried ⇒
      // We don't care about synchronization
      tried match {
        case Success(message) ⇒
          messageByteCount += message.size
          failure = None
        case Failure(t) ⇒
          failure = Some(t)
      }
      currentRequestIssuedAt = None
    }
  }
}
