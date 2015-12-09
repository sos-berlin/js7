package com.sos.scheduler.engine.tunnel.server

import akka.util.ByteString
import com.sos.scheduler.engine.http.server.heartbeat.HeartbeatService
import java.time.Duration
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */

trait TunnelAccess {
  def heartbeatService: HeartbeatService
  def execute(requestMessage: ByteString, timeout: Option[Duration]): Future[ByteString]
}
