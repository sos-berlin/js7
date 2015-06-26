package com.sos.scheduler.engine.agent.tunnel

import akka.actor.{ActorSystem, Props}
import akka.io.Tcp.Bound
import akka.pattern.ask
import akka.util.Timeout
import com.sos.scheduler.engine.agent.tunnel.TunnelHandler._
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.time.ScalaTime._

/**
 * @author Joacim Zschimmer
 */
final class TunnelHandler(actorSystem: ActorSystem) extends AutoCloseable {

  private val relaisHandler = actorSystem.actorOf( Props { new RelaisHandler }, name = "RelaisHandler")
  val tcpAddress = {
    val future = (relaisHandler ? RelaisHandler.Start)(AskTimeout).mapTo[Bound]
    awaitResult(future, ShortTimeout).localAddress
  }

  def close(): Unit = relaisHandler ! RelaisHandler.Close

  def newTunnel(tunnelId: TunnelId) = {
    awaitResult((relaisHandler ? RelaisHandler.NewTunnel(tunnelId))(AskTimeout), ShortTimeout)
    new TunnelClient(relaisHandler, tunnelId)
  }
}

object TunnelHandler {
  private val ShortTimeout = 60.s
  private val AskTimeout = Timeout(ShortTimeout.toFiniteDuration)
}
