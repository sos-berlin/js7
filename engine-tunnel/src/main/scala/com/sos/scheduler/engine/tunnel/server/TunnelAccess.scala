package com.sos.scheduler.engine.tunnel.server

import akka.util.ByteString
import com.sos.scheduler.engine.http.server.idempotence.Idempotence
import java.time.Duration
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */

trait TunnelAccess {
  def idempotence: Idempotence
  def execute(requestMessage: ByteString, timeout: Option[Duration]): Future[ByteString]
}
