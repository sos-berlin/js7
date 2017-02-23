package com.sos.jobscheduler.tunnel.server

import akka.util.ByteString
import com.sos.jobscheduler.http.server.heartbeat.HeartbeatService
import com.sos.jobscheduler.tunnel.data.{TunnelId, TunnelToken, TunnelView}
import java.net.{InetAddress, InetSocketAddress}
import java.time.{Duration, Instant}
import scala.concurrent.Future

/**
 * A handle for the server side part of a tunnel, provided by the [[TunnelServer]].
 * This is not a handle for the complete tunnel.
 *
 * @author Joacim Zschimmer
 */
trait TunnelHandle extends AutoCloseable {

  final def id: TunnelId = tunnelToken.id

  def heartbeatService: HeartbeatService

  def request(request: ByteString, timeout: Option[Duration]): Future[ByteString]

  def onInactivity(callback: Instant ⇒ Unit): Unit

  def tunnelToken: TunnelToken

  def startedByHttpIpOption: Option[InetAddress]

  def connected: Future[InetSocketAddress]

  def view: TunnelView
}
