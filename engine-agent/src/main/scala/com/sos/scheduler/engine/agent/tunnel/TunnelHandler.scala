package com.sos.scheduler.engine.agent.tunnel

import akka.actor.{ActorSystem, Props}
import akka.io.Tcp.Bound
import akka.pattern.ask
import akka.util.Timeout
import com.sos.scheduler.engine.agent.tunnel.TunnelHandler._
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.net.InetSocketAddress
import scala.concurrent.Promise
import scala.util.Try

/**
 * @author Joacim Zschimmer
 */
final class TunnelHandler(actorSystem: ActorSystem) extends AutoCloseable {

  private val relaisHandler = actorSystem.actorOf( Props { new RelaisHandler }, name = "TunnelHandler")

  val localAddress = {
    val future = (relaisHandler ? RelaisHandler.Start)(AskTimeout).mapTo[Try[Bound]]
    awaitResult(future, ShortTimeout).get.localAddress
  }

  def close(): Unit = actorSystem.stop(relaisHandler)

  def newTunnel(tunnelId: TunnelId) = {
    val connectedPromise = Promise[InetSocketAddress]()
    val password = awaitResult(
      (relaisHandler ? RelaisHandler.NewTunnel(tunnelId, connectedPromise))(AskTimeout).mapTo[Try[TunnelId.Password]],
      ShortTimeout).get
    new TunnelClient(
      relaisHandler,
      TunnelId.WithPassword(tunnelId, password),
      connectedPromise.future,
      peerAddress = () ⇒ connectedPromise.future.value map { _.get })
  }

  override def toString = s"TunnelHandler($localAddress)"
}

object TunnelHandler {
  private val ShortTimeout = 60.s
  private val AskTimeout = Timeout(ShortTimeout.toFiniteDuration)
}
