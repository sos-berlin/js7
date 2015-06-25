package com.sos.scheduler.engine.tunnel

import akka.actor.{ActorSystem, Props}
import akka.io.Tcp.Bound
import akka.pattern.ask
import akka.util.Timeout
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.tunnel.TunnelHandler._
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
    awaitResult(
      (relaisHandler ? RelaisHandler.NewTunnel(tunnelId))(AskTimeout).mapTo[Try[TunnelClient]],
      ShortTimeout).get
  }

  override def toString = s"TunnelHandler($localAddress)"
}

object TunnelHandler {
  private val ShortTimeout = 30.s
  private val AskTimeout = Timeout(ShortTimeout.toFiniteDuration)
}
