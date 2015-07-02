package com.sos.scheduler.engine.tunnel

import akka.actor.SupervisorStrategy.stoppingStrategy
import akka.actor.{Actor, ActorRef, Props}
import akka.io.{IO, Tcp}
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.tunnel.ConnectorHandler._
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import java.net.InetSocketAddress
import scala.collection.mutable
import scala.concurrent.Promise
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
 * @author Joacim Zschimmer
 */
private[tunnel] final class ConnectorHandler extends Actor {

  import context.{become, stop, system}

  override def supervisorStrategy = stoppingStrategy

  private val connectorRegister = mutable.Map[TunnelId, Entry]() withDefault { o ⇒ throw new NoSuchElementException(s"Unknown $o")}

  TunnelToken.newSecret()  // Check random generator

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
      context.actorOf(Props { new Connector(tcp, connected) }, name = s"Connector-TCP-$peerInterface:$peerPort")

    case m @ NewTunnel(id) ⇒
      logger.trace(s"$m")
      sender() ! Try[TunnelClient] {
        val secret = TunnelToken.newSecret()
        val connectedPromise = Promise[InetSocketAddress]()
        val client = new TunnelClient(
          self,
          TunnelToken(id, secret),
          connectedPromise.future,
          peerAddress = () ⇒ connectedPromise.future.value map { _.get })
        connectorRegister.insert(id → Entry(secret, client, connectedPromise, Uninitialized))
        client
      }

    case m @ Connector.AssociatedWithTunnelId(TunnelToken(id, callersSecret), peerAddress) ⇒
      logger.trace(s"$m")
      val connector = sender()
      connectorRegister.get(id) match {
        case None ⇒
          logger.error(s"Unknown TunnelId '$id' received from $connector")
          stop(connector)
        case Some(Entry(pass, _, connectedPromise, tunnelState)) ⇒
          if (callersSecret != pass) {
            logger.error(s"Invalid tunnel secret from $connector")
            stop(connector)
          } else
            tunnelState match {
              case o: ConnectedConnector ⇒
                logger.error(s"$tunnelState connects twice?")
                stop(connector)
              case Uninitialized ⇒
              case RequestBeforeConnected(request) ⇒ connector ! request
            }
          connectedPromise.success(peerAddress)
          val e = connectorRegister(id)
          e.tunnelState = ConnectedConnector(connector)
          logger.debug(s"$id connected with ${e.remoteAddressString} ")
      }

    case m @ Connector.Closed(tunnelId) ⇒
      connectorRegister -= tunnelId
      logger.debug(s"$tunnelId deregistered")

    case m @ DirectedRequest(tunnelToken, request) ⇒
      try {
        logger.trace(s"$m")
        val e = checkedEntry(tunnelToken)
        e.tunnelState match {
          case Uninitialized ⇒ e.tunnelState = RequestBeforeConnected(request)
          case o: RequestBeforeConnected ⇒ sys.error(o.toString)
          case ConnectedConnector(relais) ⇒ relais ! request
        }
      } catch {
        case NonFatal(t) ⇒
          logger.debug(s"ERROR: $m: $t", t)
          request.responsePromise.failure(t)
      }

    case m @ CloseTunnel(tunnelToken) ⇒
      try {
        if (connectorRegister contains tunnelToken.id) {
          val e = checkedEntry(tunnelToken)
          connectorRegister.remove(tunnelToken.id)
          e.tunnelState match {
            case ConnectedConnector(relais) ⇒ relais ! Connector.Close
            case _ ⇒
          }
        }
      }
      catch {
        case NonFatal(t) ⇒ logger.error(s"$m: $t", t)
      }
  }

  private def checkedEntry(tunnelToken: TunnelToken) = {
    val entry = connectorRegister(tunnelToken.id)
    if (tunnelToken.secret != entry.secret) throw new IllegalArgumentException(s"Wrong tunnel secret")
    entry
  }
}

private[tunnel] object ConnectorHandler {
  private val LocalInterface = "127.0.0.1"
  private val logger = Logger(getClass)

  private[tunnel] case object Start
  private[tunnel] case class NewTunnel(tunnelId: TunnelId)
  private[tunnel] final case class CloseTunnel(tunnelToken: TunnelToken)

  private[tunnel] final case class DirectedRequest private[tunnel](tunnelToken: TunnelToken, request: Connector.Request)

  private[tunnel] object DirectedRequest {
    def apply(tunnelToken: TunnelToken, message: ByteString, responsePromise: Promise[ByteString]): DirectedRequest =
      DirectedRequest(tunnelToken, Connector.Request(message, responsePromise))
  }

  private case class Entry(
    secret: TunnelToken.Secret,
    client: TunnelClient,
    connectedPromise: Promise[InetSocketAddress],
    var tunnelState: TunnelState)
  {
    def remoteAddressString: String = connectedPromise.future.value flatMap { _.toOption } map { _.toString } getOrElse "(not connected via TCP)"
  }

  private sealed trait TunnelState
  private case object Uninitialized extends TunnelState
  private case class RequestBeforeConnected(request: Connector.Request) extends TunnelState
  private case class ConnectedConnector(relais: ActorRef) extends TunnelState
}
