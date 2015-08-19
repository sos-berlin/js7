package com.sos.scheduler.engine.tunnel.client

import akka.actor.ActorSystem
import com.sos.scheduler.engine.common.tcp.TcpToRequestResponse
import com.sos.scheduler.engine.tunnel.client.TcpToHttpBridge._
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import java.net.InetSocketAddress
import spray.http.Uri

/**
 * A remote task started by [[com.sos.scheduler.engine.client.agent.HttpRemoteProcessStarter]], with API calls tunnelled via HTTP and Agent.
 *
 * @author Joacim Zschimmer
 */
final class TcpToHttpBridge(actorSystem: ActorSystem, connectTo: InetSocketAddress, tunnelToken: TunnelToken, tunnelClient: WebTunnelClient)
extends AutoCloseable {
  def this(actorSystem: ActorSystem, connectTo: InetSocketAddress, tunnelToken: TunnelToken, baseUri: Uri) =
    this(actorSystem, connectTo, tunnelToken, newWebTunnelClient(actorSystem, baseUri))

  protected def executionContext = actorSystem.dispatcher

  private val tcpHttpBridge = new TcpToRequestResponse(
    actorSystem,
    connectTo = connectTo,
    executeRequest = request ⇒ tunnelClient.tunnelRequest(tunnelToken, request))

  def start() = tcpHttpBridge.start()

  def close() = tcpHttpBridge.close()
}

object TcpToHttpBridge {
  private def newWebTunnelClient(pActorSystem: ActorSystem, baseUri: Uri) = new WebTunnelClient {
    protected def actorSystem = pActorSystem
    protected def tunnelUri(id: TunnelId) = baseUri withPath (baseUri.path / id.string)
  }
}
