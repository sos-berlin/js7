package com.sos.scheduler.engine.tunnel.server

import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import java.net.{InetAddress, InetSocketAddress}
import scala.concurrent.Future

/**
 * A handle for the server side part of a tunnel, provided by the [[TunnelServer]].
 * This is not a handle for the complete tunnel.
 *
 * @author Joacim Zschimmer
 */
trait TunnelHandle extends AutoCloseable {
  final def id: TunnelId = tunnelToken.id

  def tunnelToken: TunnelToken
  def startedByHttpIpOption: Option[InetAddress]
  def connected: Future[InetSocketAddress]
}
