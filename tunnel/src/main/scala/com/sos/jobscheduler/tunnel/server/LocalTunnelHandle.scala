package com.sos.jobscheduler.tunnel.server

import akka.util.ByteString
import com.sos.jobscheduler.common.scalautil.Futures.NoFuture
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.http.server.heartbeat.HeartbeatService
import com.sos.jobscheduler.tunnel.data.{TunnelStatistics, TunnelToken, TunnelView}
import java.net.InetAddress
import java.time.{Duration, Instant}
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
final class LocalTunnelHandle(
  val tunnelToken: TunnelToken,
  onRequest: ByteString ⇒ Future[ByteString],
  val startedByHttpIpOption: Option[InetAddress])
  (implicit timerService: TimerService,
  executionContext: ExecutionContext)
extends TunnelHandle {

  val heartbeatService = new HeartbeatService
  private val statistics = new Statistics()

  def request(requestMessage: ByteString, timeout: Option[Duration]) = {
    require(timeout.isEmpty, "LocalTunnelHandle only without timeout")  // timeout is for HTTP tunnel heartbeat
    onRequest(requestMessage)
  }

  /** LocalTunnelHandle does not detect inactivity, because the connetion will not break. */
  def onInactivity(callback: (Instant) ⇒ Unit) = {}

  def close() = {}

  def connected = NoFuture

  def view = TunnelView(
    id,
    startedAt,
    startedByHttpIpOption,
    remoteTcpAddress = None,
    heartbeatService.overview,
    TunnelStatistics(
      requestCount = statistics.requestCount,
      messageByteCount = statistics.messageByteCount,
      currentRequestIssuedAt = statistics.currentRequestIssuedAt,
      statistics.failure map { _.toString })  )

  override def toString = s"LocalTunnelHandle($tunnelToken)"
}
