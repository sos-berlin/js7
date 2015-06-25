package com.sos.scheduler.engine.tunnel

import akka.actor.SupervisorStrategy.stoppingStrategy
import akka.actor.{Actor, ActorRef, Props}
import akka.io.{IO, Tcp}
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.tunnel.RelaisHandler._
import java.net.InetSocketAddress
import scala.collection.mutable
import scala.concurrent.Promise
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
 * @author Joacim Zschimmer
 */
private[tunnel] final class RelaisHandler extends Actor {

  import context.{become, stop, system}

  override def supervisorStrategy = stoppingStrategy
  TunnelId.newPassword()  // Check random generator

  // TODO Entry entfernen, nur wenn Relais sich beendet hat
  // TODO Auskunft
  private val relaisRegister = mutable.Map[TunnelId, Entry]()

  def receive = {
    case Start ⇒
      IO(Tcp) ! Tcp.Bind(self, new InetSocketAddress(LocalInterface, 0))
      become(starting(sender()))
  }

  def starting(respondTo: ActorRef): Receive = {
    case bound: Tcp.Bound ⇒
      logger.debug(s"$bound")
      respondTo ! Success(bound)
      become(ready)

    case m @ Tcp.CommandFailed(_: Tcp.Bind) ⇒
      respondTo ! Failure(new RuntimeException(s"A TCP port could not be bound: $m"))
      stop(self)
  }

  private def ready: Receive = {
    case connected: Tcp.Connected ⇒
      logger.debug(s"$connected")
      val tcp = sender()
      val peerInterface = connected.remoteAddress.getAddress.getHostAddress
      val peerPort = connected.remoteAddress.getPort
      context.actorOf(Props { new Relais(tcp, connected.remoteAddress) }, name = s"Relais-TCP-$peerInterface:$peerPort")

    case m @ NewTunnel(id) ⇒
      logger.trace(s"$m")
      sender() ! Try[TunnelClient] {
        val password = TunnelId.newPassword()
        val connectedPromise = Promise[InetSocketAddress]()
        val client = new TunnelClient(
          self,
          TunnelId.WithPassword(id, password),
          connectedPromise.future,
          peerAddress = () ⇒ connectedPromise.future.value map { _.get })
        relaisRegister.insert(id → Entry(password, client, connectedPromise, Uninitialized))
        client
      }

    case m @ Relais.RelaisAssociatedWithTunnelId(TunnelId.WithPassword(id, callersPassword), peerAddress) ⇒
      logger.trace(s"$m")
      val relais = sender()
      relaisRegister.get(id) match {
        case None ⇒
          logger.error(s"Unknown TunnelId '$id' received from $relais")
          stop(relais)
        case Some(Entry(pass, _, connectedPromise, tunnelState)) ⇒
          if (callersPassword != pass) {
            logger.error(s"Invalid tunnel password from $relais")
            stop(relais)
          } else
            tunnelState match {
              case o: ConnectedRelais ⇒
                logger.error(s"$tunnelState connects twice?")
                stop(relais)
              case Uninitialized ⇒
              case RequestBeforeConnected(request) ⇒ relais ! request
            }
          connectedPromise.success(peerAddress)
          relaisRegister(id).tunnelState = ConnectedRelais(relais)
      }

    case m @ DirectedRequest(idWithPassword, request) ⇒
      try {
        logger.trace(s"$m")
        val e = checkedEntry(idWithPassword)
        e.tunnelState match {
          case Uninitialized ⇒ e.tunnelState = RequestBeforeConnected(request)
          case o: RequestBeforeConnected ⇒ sys.error(o.toString)
          case ConnectedRelais(relais) ⇒ relais ! request
        }
      } catch {
        case NonFatal(t) ⇒
          logger.debug(s"ERROR: $m: $t", t)
          request.responsePromise.failure(t)
      }

    case m @ CloseTunnel(idWithPassword) ⇒
      try {
        if (relaisRegister contains idWithPassword.id) {
          val e = checkedEntry(idWithPassword)
          relaisRegister.remove(idWithPassword.id)
          e.tunnelState match {
            case ConnectedRelais(relais) ⇒ relais ! Relais.Close
            case _ ⇒
          }
        }
      }
      catch {
        case NonFatal(t) ⇒ logger.error(s"$m: $t", t)
      }
  }

  private def checkedEntry(idWithPassword: TunnelId.WithPassword) = {
    val entry = relaisRegister(idWithPassword.id)
    if (idWithPassword.password != entry.password) throw new IllegalArgumentException(s"Invalid tunnel password")
    entry
  }

}

private[tunnel] object RelaisHandler {
  private val LocalInterface = "127.0.0.1"
  private val logger = Logger(getClass)

  private[tunnel] case object Start
  private[tunnel] case class NewTunnel(tunnelId: TunnelId)
  private[tunnel] final case class CloseTunnel(tunnelIdWithPassword: TunnelId.WithPassword)

  private case class Entry(
    password: TunnelId.Password,
    client: TunnelClient,
    connectedPromise: Promise[InetSocketAddress],
    var tunnelState: TunnelState)

  private sealed trait TunnelState
  private case object Uninitialized extends TunnelState
  private case class RequestBeforeConnected(request: Request) extends TunnelState
  private case class ConnectedRelais(relais: ActorRef) extends TunnelState
}
