package com.sos.scheduler.engine.tunnel

import akka.actor.{ActorSystem, Props}
import akka.io.Tcp.Bound
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.tunnel.TunnelHandler._
import com.sos.scheduler.engine.tunnel.core.{ConnectorHandler, TunnelClient}
import com.sos.scheduler.engine.tunnel.data.{TunnelHandlerOverview, TunnelId, TunnelOverview, TunnelToken}
import java.net.InetSocketAddress
import javax.inject.{Inject, Singleton}
import scala.collection.immutable
import scala.concurrent.{Future, Promise}
import scala.util.Try

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class TunnelHandler @Inject private[tunnel](actorSystem: ActorSystem) extends AutoCloseable {

  private val connectorHandler = actorSystem.actorOf( Props { new ConnectorHandler }, name = "ConnectorHandler")
  private implicit val askTimeout = Timeout(ShortTimeout.toFiniteDuration)

  private[tunnel] val proxyAddress: InetSocketAddress = {
    val future = (connectorHandler ? ConnectorHandler.Start).mapTo[Try[Bound]]
    awaitResult(future, ShortTimeout).get.localAddress
  }
  /**
   * The TCP address "ipnumber:port" of the proxy, which tunnels the data to the real master.
   */
  val proxyAddressString = proxyAddress.getAddress.getHostAddress +":"+ proxyAddress.getPort

  def close(): Unit = actorSystem.stop(connectorHandler)

  def newTunnel(tunnelId: TunnelId) = {
    awaitResult((connectorHandler ? ConnectorHandler.NewTunnel(tunnelId)).mapTo[Try[TunnelClient]],
      ShortTimeout).get
  }

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

object TunnelHandler {
  private val ShortTimeout = 30.s
}
