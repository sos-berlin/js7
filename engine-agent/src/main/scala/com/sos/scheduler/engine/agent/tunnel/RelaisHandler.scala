package com.sos.scheduler.engine.agent.tunnel

import akka.actor.SupervisorStrategy.stoppingStrategy
import akka.actor.{Actor, ActorRef, Props}
import akka.io.{IO, Tcp}
import com.sos.scheduler.engine.agent.tunnel.Relais.{DirectedRequest, RelaisConnected}
import com.sos.scheduler.engine.agent.tunnel.RelaisHandler._
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import java.net.InetSocketAddress
import scala.collection.mutable
import scala.util.control.NonFatal
import scala.util.{Failure, Try}

/**
 * @author Joacim Zschimmer
 */
final class RelaisHandler extends Actor {

  import context.{become, system}

  override def supervisorStrategy = stoppingStrategy

  private val relaisRegister = mutable.Map[TunnelId, RelaisState]()

  def receive = {
    case Start ⇒
      IO(Tcp) ! Tcp.Bind(self, new InetSocketAddress("127.0.0.1", 0))
      context.become(starting(sender()))
  }

  def starting(respondTo: ActorRef): Receive = {
    case bound: Tcp.Bound ⇒
      logger.debug(s"$bound")
      respondTo ! bound
      become(ready)

    case Tcp.CommandFailed(_: Tcp.Bind) ⇒
      context.parent ! Failure(new RuntimeException("A TCP port could not be bound"))
      context.stop(self)
  }

  private def ready: Receive = {
    case connected: Tcp.Connected ⇒
      logger.debug(s"$connected")
      val tcpConnection = sender()
      context.actorOf(Props { new Relais(tcpConnection, connected.remoteAddress) },
        name = s"Relais-TCP-${connected.remoteAddress.getAddress.getHostAddress}:${connected.remoteAddress.getPort}")

    case m @ NewTunnel(id) ⇒
      logger.debug(s"$m")
      sender() ! Try[Unit] { relaisRegister.insert(id → New) }

    case m @ RelaisConnected(id) ⇒
      logger.debug(s"$m")
      val relais = sender()
      relaisRegister(id) match {
        case New ⇒
        case RequestBeforeConnected(request) ⇒ relais ! request
        case o: ConnectedRelais ⇒ sys.error(o.toString)
      }
      relaisRegister(id) = ConnectedRelais(relais)

    case directedRequest @ DirectedRequest(id, request, _) ⇒
      logger.trace(s"Request($id, ${request.size} bytes})")
      relaisRegister(id) match {
        case New ⇒ relaisRegister(id) = RequestBeforeConnected(directedRequest)
        case o: RequestBeforeConnected ⇒ sys.error(o.toString)
        case ConnectedRelais(relais) ⇒ relaisRegister(id).asInstanceOf[ConnectedRelais].relais ! directedRequest
      }

    case m @ CloseTunnel(id) ⇒
      try for (ConnectedRelais(relais) ← relaisRegister.get(id)) relais ! Relais.Close
      catch {
        case NonFatal(t) ⇒ logger.error(s"$m: $t", t)
      }

    case Close ⇒  // TODO In jedem Zustand zulassen, nicht nur ready
      context.stop(self)
  }
}

object RelaisHandler {
  private val logger = Logger(getClass)
  private[tunnel] case object Start
  private[tunnel] case object Close
  private[tunnel] case class NewTunnel(tunnelId: TunnelId)
  private[tunnel] final case class CloseTunnel(tunnelId: TunnelId)

  private sealed trait RelaisState
  private case object New extends RelaisState
  private case class RequestBeforeConnected(request: DirectedRequest) extends RelaisState
  private case class ConnectedRelais(relais: ActorRef) extends RelaisState
}
