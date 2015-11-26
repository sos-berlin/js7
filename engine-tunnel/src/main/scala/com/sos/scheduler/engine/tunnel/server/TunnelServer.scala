package com.sos.scheduler.engine.tunnel.server

import akka.actor.ActorSystem
import akka.agent.Agent
import akka.io.Tcp.Bound
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.http.server.heartbeat.HeartbeatTimeout
import com.sos.scheduler.engine.tunnel.data.{TunnelHandlerOverview, TunnelId, TunnelOverview, TunnelToken}
import com.sos.scheduler.engine.tunnel.server.TunnelServer._
import java.net.{InetAddress, InetSocketAddress}
import javax.inject.{Inject, Singleton}
import scala.collection.immutable
import scala.concurrent.{Future, Promise}
import scala.util.Try

/**
 * Server for all tunnels.
 * Provides methods for creating tunnels and forwards requests (received via HTTP).
 * Frontend for [[ConnectorHandler]].
 *
 * @author Joacim Zschimmer
 */
@Singleton
final class TunnelServer @Inject private[tunnel](actorSystem: ActorSystem, conf: TunnelConfiguration = TunnelConfiguration()) extends AutoCloseable {

  private val connectorHandler = actorSystem.actorOf(ConnectorHandler.props(inactivityTimeout = conf.inactivityTimeout), name = "ConnectorHandler")
  private implicit val askTimeout = Timeout(ShortTimeout.toFiniteDuration)

  private[tunnel] val proxyAddress: InetSocketAddress = {
    val future = (connectorHandler ? ConnectorHandler.Start).mapTo[Try[Bound]]
    awaitResult(future, ShortTimeout).get.localAddress
  }
  /**
   * The TCP address "ipNumber:port" of the proxy, which tunnels the data to the real master.
   */
  val proxyAddressString = proxyAddress.getAddress.getHostAddress +":"+ proxyAddress.getPort

  def close(): Unit = actorSystem.stop(connectorHandler)

  /**
   * @param startedByIpOption Only for information
   */
  def newTunnel[A <: TunnelListener](tunnelId: TunnelId, tunnelListener: Agent[A], startedByIpOption: Option[InetAddress] = None): TunnelHandle =
    awaitResult((connectorHandler ? ConnectorHandler.NewTunnel(tunnelId, tunnelListener, startedByIpOption)).mapTo[Try[TunnelHandle]],
      ShortTimeout).get

  def onHeartbeatTimeout(tunnelToken: TunnelToken, t: HeartbeatTimeout): Unit =
    awaitResult((connectorHandler ? ConnectorHandler.OnHeartbeatTimeout(tunnelToken, t)).mapTo[Try[TunnelHandle]],
      ShortTimeout).get

  def request(tunnelToken: TunnelToken, requestMessage: ByteString): Future[ByteString] = {
    val responsePromise = Promise[ByteString]()
    connectorHandler ! ConnectorHandler.DirectedRequest(tunnelToken, requestMessage, responsePromise)
    responsePromise.future
  }

  override def toString = s"TunnelHandler($proxyAddress)"

  def overview: Future[TunnelHandlerOverview] =
    (connectorHandler ? ConnectorHandler.GetOverview).mapTo[TunnelHandlerOverview]

  def tunnelOverviews: Future[immutable.Iterable[TunnelOverview]] =
    (connectorHandler ? ConnectorHandler.GetTunnelOverviews).mapTo[immutable.Iterable[TunnelOverview]]
}

object TunnelServer {
  private val ShortTimeout = 30.s
}
