package com.sos.scheduler.engine.tunnel.server

import akka.actor.ActorSystem
import akka.agent.Agent
import akka.io.Tcp.Bound
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.tunnel.data._
import com.sos.scheduler.engine.tunnel.server.TunnelServer._
import java.net.{InetAddress, InetSocketAddress}
import java.time.Duration
import javax.inject.{Inject, Singleton}
import scala.collection.immutable
import scala.concurrent.Future
import scala.util.Try

/**
 * Server for all tunnels.
 * Provides methods for creating tunnels and forwards requests (received via HTTP).
 * Frontend for [[ConnectorHandler]].
 *
 * @author Joacim Zschimmer
 */
@Singleton
final class TunnelServer @Inject private[tunnel](actorSystem: ActorSystem)(implicit timerService: TimerService) extends AutoCloseable {

  private val connectorHandler = actorSystem.actorOf(ConnectorHandler.props(timerService), name = "ConnectorHandler")
  private implicit val askTimeout = Timeout(ShortTimeout.toFiniteDuration)

  import actorSystem.dispatcher

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

  def onHeartbeat(tunnelToken: TunnelToken, timeout: Duration): Unit = {
    (connectorHandler ? ConnectorHandler.OnHeartbeat(tunnelToken, timeout)).mapTo[Try[Unit]]
  }

  def tunnelAccess(tunnelToken: TunnelToken) = new TunnelAccess {
    private val tunnel = handle(tunnelToken)
    val heartbeatService = tunnel.heartbeatService
    def execute(requestMessage: ByteString, timeout: Option[Duration]) = tunnel.request(requestMessage, timeout)
  }

  def handle(tunnelToken: TunnelToken): TunnelHandle =
    awaitResult((connectorHandler ? ConnectorHandler.GetHandle(tunnelToken)).mapTo[Try[TunnelHandle]], ShortTimeout).get

  override def toString = s"TunnelHandler($proxyAddress)"

  def overview: Future[TunnelHandlerOverview] =
    (connectorHandler ? ConnectorHandler.GetOverview).mapTo[Try[TunnelHandlerOverview]] map { _.get }

  def tunnelOverviews: Future[immutable.Iterable[TunnelOverview]] =
    (connectorHandler ? ConnectorHandler.GetTunnelOverviews).mapTo[Try[immutable.Iterable[TunnelOverview]]] map { _.get }

  def tunnelView(tunnelId: TunnelId): Future[TunnelView] =
    (connectorHandler ? ConnectorHandler.GetTunnelView(tunnelId)).mapTo[Try[TunnelView]] map { _.get }
}

object TunnelServer {
  private val ShortTimeout = 30.s
}
