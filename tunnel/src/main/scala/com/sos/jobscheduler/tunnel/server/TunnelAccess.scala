package com.sos.jobscheduler.tunnel.server

import akka.util.ByteString
import com.sos.jobscheduler.http.server.heartbeat.HeartbeatService
import java.time.Duration
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */

trait TunnelAccess {
  def heartbeatService: HeartbeatService
  def execute(requestMessage: ByteString, timeout: Option[Duration]): Future[ByteString]
}
