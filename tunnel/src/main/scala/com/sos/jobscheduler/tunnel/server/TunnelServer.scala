package com.sos.jobscheduler.tunnel.server

import akka.actor.ActorSystem
import akka.agent.Agent
import akka.io.Tcp.Bound
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import com.sos.jobscheduler.common.scalautil.Futures._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.tunnel.data._
import com.sos.jobscheduler.tunnel.server.TunnelServer._
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

  import actorSystem.dispatcher

  private implicit val askTimeout = Timeout(ShortTimeout.toFiniteDuration)
  private var hasProxyAddress = false
  private lazy val (connectorHandler, _proxyAddress) = {
    val actor = actorSystem.actorOf(ConnectorHandler.props(timerService), name = "ConnectorHandler")
    val future = (actor ? ConnectorHandler.Start).mapTo[Try[Bound]]
    val a = awaitResult(future, ShortTimeout).get.localAddress
    hasProxyAddress = true
    logger.debug(s"Listening on TCP port $a for tunnel end-points")
    (actor, a)
  }

  private[tunnel] def proxyAddress: InetSocketAddress = _proxyAddress
  
  /**
   * The TCP address "ipNumber:port" of the proxy, which tunnels the data to the real master.
   */
  lazy val proxyAddressString = proxyAddress.getAddress.getHostAddress +":"+ proxyAddress.getPort

  def close(): Unit = actorSystem.stop(connectorHandler)

  /**
   * @param startedByIpOption Only for information
   */
  def newTunnel[A <: TunnelListener](tunnelToken: TunnelToken, tunnelListener: Agent[A], startedByIpOption: Option[InetAddress] = None): TunnelHandle =
    awaitResult((connectorHandler ? ConnectorHandler.NewTunnel(tunnelToken, tunnelListener, startedByIpOption)).mapTo[Try[TunnelHandle]],
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

  override def toString = s"TunnelHandler(${if (hasProxyAddress) proxyAddress else ""})"

  def overview: Future[TunnelHandlerOverview] =
    (connectorHandler ? ConnectorHandler.GetOverview).mapTo[Try[TunnelHandlerOverview]] map { _.get }

  def tunnelOverviews: Future[immutable.Iterable[TunnelOverview]] =
    (connectorHandler ? ConnectorHandler.GetTunnelOverviews).mapTo[Try[immutable.Iterable[TunnelOverview]]] map { _.get }

  def tunnelView(tunnelId: TunnelId): Future[TunnelView] =
    (connectorHandler ? ConnectorHandler.GetTunnelView(tunnelId)).mapTo[Try[TunnelView]] map { _.get }
}

object TunnelServer {
  private val ShortTimeout = 30.s
  private val logger = Logger(getClass)
}
