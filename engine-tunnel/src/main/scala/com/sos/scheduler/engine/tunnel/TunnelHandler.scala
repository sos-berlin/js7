package com.sos.scheduler.engine.tunnel

import akka.actor.{ActorSystem, Props}
import akka.io.Tcp.Bound
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.tunnel.TunnelHandler._
import com.sos.scheduler.engine.tunnel.core.{ConnectorHandler, TunnelClient}
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import javax.inject.{Inject, Singleton}
import scala.concurrent.{Future, Promise}
import scala.util.Try

/**
 * @author Joacim Zschimmer
 */
@Singleton
final class TunnelHandler @Inject private[tunnel](actorSystem: ActorSystem) extends AutoCloseable {

  private val connectorHandler = actorSystem.actorOf( Props { new ConnectorHandler }, name = "ConnectorHandler")

  val localAddress = {
    val future = (connectorHandler ? ConnectorHandler.Start)(AskTimeout).mapTo[Try[Bound]]
    awaitResult(future, ShortTimeout).get.localAddress
  }

  def close(): Unit = actorSystem.stop(connectorHandler)

  def newTunnel(tunnelId: TunnelId) = {
    awaitResult(
      (connectorHandler ? ConnectorHandler.NewTunnel(tunnelId))(AskTimeout).mapTo[Try[TunnelClient]],
      ShortTimeout).get
  }

  def request(tunnelToken: TunnelToken, requestMessage: ByteString): Future[ByteString] = {
    val responsePromise = Promise[ByteString]()
    connectorHandler ! ConnectorHandler.DirectedRequest(tunnelToken, requestMessage, responsePromise)
    responsePromise.future
  }

  override def toString = s"TunnelHandler($localAddress)"
}

object TunnelHandler {
  private val ShortTimeout = 30.s
  private val AskTimeout = Timeout(ShortTimeout.toFiniteDuration)
}
