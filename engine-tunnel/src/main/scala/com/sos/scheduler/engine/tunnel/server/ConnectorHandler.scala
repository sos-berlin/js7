package com.sos.scheduler.engine.tunnel.server

import akka.actor.SupervisorStrategy.stoppingStrategy
import akka.actor.{Actor, ActorRef, Props, Terminated}
import akka.agent.Agent
import akka.io.{IO, Tcp}
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.{Logger, SetOnce}
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.tunnel.data._
import com.sos.scheduler.engine.tunnel.server.ConnectorHandler._
import java.net.{InetAddress, InetSocketAddress}
import java.time.Duration
import scala.PartialFunction.cond
import scala.collection.mutable
import scala.concurrent.Promise
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
 * @author Joacim Zschimmer
 */
private[tunnel] final class ConnectorHandler private(implicit timerService: TimerService) extends Actor {

  import context.{actorOf, become, dispatcher, stop, system, watch}

  override def supervisorStrategy = stoppingStrategy

  private val register = mutable.Map[TunnelId, Handle]() withDefault { id ⇒ throw new NoSuchElementException(s"Unknown $id") }
  private val tcpAddressOnce = new SetOnce[InetSocketAddress]

  TunnelToken.newSecret()  // Check random generator

  def receive = {
    case Start ⇒
      IO(Tcp) ! Tcp.Bind(self, new InetSocketAddress(LocalInterface, 0))
      become(starting(sender()))
  }

  def starting(respondTo: ActorRef): Receive = {
    case bound: Tcp.Bound ⇒
      logger.debug(s"$bound")
      tcpAddressOnce := bound.localAddress
      respondTo ! Success(bound)
      become(ready)

    case m @ Tcp.CommandFailed(_: Tcp.Bind) ⇒
      respondTo ! Failure(new RuntimeException(s"A TCP port could not be bound: $m"))
      stop(self)
  }

  private def ready: Receive = {
    case connected: Tcp.Connected ⇒
      logger.debug(s"$connected")
      val name = {
        val peerInterface = connected.remoteAddress.getAddress.getHostAddress
        val peerPort = connected.remoteAddress.getPort
        s"Connector-TCP-$peerInterface:$peerPort"
      }
      val connector = actorOf(Connector.props(connectorHandler = self, tcp = sender(), connected, timerService), name = name)
      watch(connector)

    case m @ NewTunnel(id, listener, startedByIpOption) ⇒
      logger.trace(s"$m")
      sender() ! Try[TunnelHandle] {
        val secret = TunnelToken.newSecret()
        val connectedPromise = Promise[InetSocketAddress]()
        val handle = new Handle(
          self,
          TunnelToken(id, secret),
          startedByHttpIpOption = startedByIpOption,
          connectedPromise,
          listener)
        register.insert(id → handle)
        handle
      }

    case m @ Connector.AssociatedWithTunnelId(TunnelToken(id, secret), peerAddress) ⇒
      logger.trace(s"$m")
      val connector = sender()
      register.get(id) match {
        case None ⇒
          logger.error(s"Unknown TunnelId '$id' received from $connector")
          stop(connector)
        case Some(handle: Handle) ⇒
          if (secret != handle.tunnelToken.secret) {
            logger.error(s"Tunnel secret from $connector does not match with $id")
            stop(connector)
          } else {
            handle.state match {
              case o: Handle.ConnectedConnector ⇒
                logger.error(s"${handle.state} has tried to connect twice with $id")
                stop(connector)
              case Handle.Uninitialized ⇒
              case Handle.RequestBeforeConnected(request) ⇒
                handle.onRequest(request)
                connector ! request
            }
          }
          handle.connectedPromise.success(peerAddress)
          handle.state = Handle.ConnectedConnector(connector)
          logger.debug(s"$id connected with ${handle.remoteAddressString}")
      }

    case m @ Terminated(child) ⇒
      for (id ← getTunnelIdByActor(child)) {
        removeHandle(id)
        logger.debug(s"$m, tunnel removed after Connector death")  // Should have been removed by CloseTunnel
      }

    case m @ DirectedRequest(tunnelToken, request) ⇒
      try {
        logger.trace(s"$m")
        val handle = checkedHandle(tunnelToken)
        handle.state match {
          case Handle.ConnectedConnector(connector) ⇒
            connector ! request
            handle.onRequest(request)
          case Handle.Uninitialized ⇒ handle.state = Handle.RequestBeforeConnected(request)
          case o: Handle.RequestBeforeConnected ⇒ sys.error("Second request before connection has been established")
        }
      } catch {
        case NonFatal(t) ⇒ request.responsePromise.failure(t)
      }

    case m @ OnHeartbeat(tunnelToken, timeout) ⇒
      sender() ! Try {
        checkedHandle(tunnelToken).state match {
          case Handle.ConnectedConnector(connector) ⇒ connector ! Connector.Heartbeat(timeout)
          case state ⇒ logger.debug(s"$m ignored in state $state")
        }
      }

    case Connector.BecameInactive(tunnelId, since) ⇒   // Timeout of last Connector.Request or Connector.Heartbeat has elapsed
      for (handle ← register.get(tunnelId)) {
        handle.callOnInactivity(since)
      }

    case m @ CloseTunnel(tunnelToken) ⇒
      try {
        if (register contains tunnelToken.id) {
          checkedHandle(tunnelToken).state match {
            case Handle.ConnectedConnector(connector) ⇒ connector ! Connector.Close
            case _ ⇒
          }
          removeHandle(tunnelToken.id)
        }
      } catch {
        case NonFatal(t) ⇒ logger.error(s"$m: $t", t)
      }

    case GetHandle(tunnelToken) ⇒
      sender() ! Try { checkedHandle(tunnelToken) }

    case GetOverview ⇒
      sender() ! Try {
        TunnelHandlerOverview(
          tcpAddress = tcpAddressOnce.get map { _.toString },
          tunnelCount = register.size)
      }

    case GetTunnelOverviews ⇒
      sender() ! Try {
        (register map { case (id, handle) ⇒
          TunnelOverview(
            id,
            handle.startedByHttpIpOption,
            handle.remoteAddressStringOption)
        }).toVector
      }

    case GetTunnelView(tunnelId) ⇒
      sender() ! Try { register(tunnelId).view }
  }

  private def removeHandle(id: TunnelId): Unit = register -= id

  private def checkedHandle(tunnelToken: TunnelToken) = {
    val handle = register(tunnelToken.id)
    if (tunnelToken.secret != handle.tunnelToken.secret) throw new IllegalArgumentException(s"Wrong tunnel secret")
    handle
  }

  private def getTunnelIdByActor(actorRef: ActorRef): Option[TunnelId] =
    register.iterator collectFirst {
      case (id, handle) if cond(handle.state) { case Handle.ConnectedConnector(connector) ⇒ connector == actorRef } ⇒ id
    }
}

private[tunnel] object ConnectorHandler {
  private val LocalInterface = "127.0.0.1"
  private val logger = Logger(getClass)

  private[tunnel] def props(implicit timerService: TimerService) = Props { new ConnectorHandler }

  sealed trait Command

  private[tunnel] case object Start extends Command

  private[tunnel] case class NewTunnel[A <: TunnelListener](
    tunnelId: TunnelId,
    listener: Agent[A],
    startedByIpOption: Option[InetAddress])
  extends Command

  private[tunnel] final case class CloseTunnel(tunnelToken: TunnelToken) extends Command

  private[tunnel] final case class GetHandle(tunnelToken: TunnelToken) extends Command

  private[tunnel] final case class GetTunnelView(tunnelId: TunnelId) extends Command

  private[tunnel] final case class OnHeartbeat(tunnelToken: TunnelToken, timeout: Duration) extends Command

  private[tunnel] final case class DirectedRequest private[tunnel](tunnelToken: TunnelToken, request: Connector.Request) extends Command

  private[tunnel] object DirectedRequest extends Command {
    def apply(tunnelToken: TunnelToken, message: ByteString, responsePromise: Promise[ByteString], timeout: Option[Duration]): DirectedRequest =
      DirectedRequest(tunnelToken, Connector.Request(message, responsePromise, timeout))
  }

  private[tunnel] object GetOverview extends Command
  private[tunnel] object GetTunnelOverviews extends Command
}
