package com.sos.scheduler.engine.tunnel.core

import akka.actor.SupervisorStrategy.stoppingStrategy
import akka.actor.{Actor, ActorRef, Props}
import akka.io.{IO, Tcp}
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.tunnel.core.ConnectorHandler._
import com.sos.scheduler.engine.tunnel.data._
import java.net.InetSocketAddress
import java.time.Instant
import java.time.Instant.now
import scala.collection.mutable
import scala.concurrent.Promise
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
 * @author Joacim Zschimmer
 */
private[tunnel] final class ConnectorHandler extends Actor {

  import context.{become, dispatcher, stop, system}

  override def supervisorStrategy = stoppingStrategy

  private val connectorRegister = mutable.Map[TunnelId, Entry]() withDefault { o ⇒ throw new NoSuchElementException(s"Unknown $o")}
  private var tcpAddressOption: Option[InetSocketAddress] = None

  TunnelToken.newSecret()  // Check random generator

  def receive = {
    case Start ⇒
      IO(Tcp) ! Tcp.Bind(self, new InetSocketAddress(LocalInterface, 0))
      become(starting(sender()))
  }

  def starting(respondTo: ActorRef): Receive = {
    case bound: Tcp.Bound ⇒
      logger.debug(s"$bound")
      tcpAddressOption = Some(bound.localAddress)
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

    case m @ Connector.AssociatedWithTunnelId(TunnelToken(id, secret), peerAddress) ⇒
      logger.trace(s"$m")
      val connector = sender()
      connectorRegister.get(id) match {
        case None ⇒
          logger.error(s"Unknown TunnelId '$id' received from $connector")
          stop(connector)
        case Some(entry: Entry) ⇒
          if (secret != entry.secret) {
            logger.error(s"Tunnel secret from $connector does not match with $id")
            stop(connector)
          } else {
            entry.tunnelState match {
              case o: ConnectedConnector ⇒
                logger.error(s"${entry.tunnelState} has tried to connect twice with $id")
                stop(connector)
              case Uninitialized ⇒
              case RequestBeforeConnected(request) ⇒ connector ! request
            }
          }
          entry.connectedPromise.success(peerAddress)
          entry.tunnelState = ConnectedConnector(connector)
          logger.debug(s"$id connected with ${entry.remoteAddressString}")
      }

    case m @ Connector.Closed(tunnelId) ⇒
      connectorRegister -= tunnelId
      logger.debug(s"$m, tunnel removed")

    case m @ DirectedRequest(tunnelToken, request) ⇒
      try {
        logger.trace(s"$m")
        val entry = checkedEntry(tunnelToken)
        entry.tunnelState match {
          case ConnectedConnector(relais) ⇒
            relais ! request
            updateStatistics(entry, request)
          case Uninitialized ⇒ entry.tunnelState = RequestBeforeConnected(request)
          case o: RequestBeforeConnected ⇒ sys.error(o.toString)
        }
      } catch {
        case NonFatal(t) ⇒ request.responsePromise.failure(t)
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

    case GetOverview ⇒
      sender() ! TunnelHandlerOverview(
        tcpAddress = tcpAddressOption map { _.toString },
        tunnelCount = connectorRegister.size)

    case GetTunnelOverviews ⇒
      sender() ! (connectorRegister map { case (id, entry) ⇒
        TunnelOverview(
          id,
          entry.remoteAddressStringOption,
          TunnelStatistics(
            requestCount = entry.statistics.requestCount,
            messageByteCount = entry.statistics.messageByteCount,
            currentRequestIssuedAt = entry.statistics.currentRequestIssuedAt,
            entry.statistics.failure map { _.toString }))
      }).toVector
  }

  private def checkedEntry(tunnelToken: TunnelToken) = {
    val entry = connectorRegister(tunnelToken.id)
    if (tunnelToken.secret != entry.secret) throw new IllegalArgumentException(s"Wrong tunnel secret")
    entry
  }

  private def updateStatistics(entry: Entry, request: Connector.Request): Unit = {
    entry.statistics.currentRequestIssuedAt = Some(now)
    entry.statistics.requestCount += 1
    entry.statistics.messageByteCount += request.message.size
    request.responsePromise.future.onComplete { tried ⇒
      // We don't care about synchronization
      tried match {
        case Success(message) ⇒
          entry.statistics.messageByteCount += message.size
          entry.statistics.failure = None
        case Failure(t) ⇒
          entry.statistics.failure = Some(t)
      }
      entry.statistics.currentRequestIssuedAt = None
    }
  }
}

private[tunnel] object ConnectorHandler {
  private val LocalInterface = "127.0.0.1"
  private val logger = Logger(getClass)

  sealed trait Command
  private[tunnel] case object Start extends Command
  private[tunnel] case class NewTunnel(tunnelId: TunnelId) extends Command
  private[tunnel] final case class CloseTunnel(tunnelToken: TunnelToken) extends Command

  private[tunnel] final case class DirectedRequest private[tunnel](tunnelToken: TunnelToken, request: Connector.Request) extends Command

  private[tunnel] object DirectedRequest {
    def apply(tunnelToken: TunnelToken, message: ByteString, responsePromise: Promise[ByteString]): DirectedRequest =
      DirectedRequest(tunnelToken, Connector.Request(message, responsePromise))
  }

  private[tunnel] object GetOverview extends Command
  private[tunnel] object GetTunnelOverviews extends Command

  private case class Entry(
    secret: TunnelToken.Secret,
    client: TunnelClient,
    connectedPromise: Promise[InetSocketAddress],
    var tunnelState: TunnelState,
    statistics: Statistics = Statistics())
  {
    def remoteAddressString: String = remoteAddressStringOption getOrElse "(not connected via TCP)"
    def remoteAddressStringOption: Option[String] = connectedPromise.future.value flatMap { _.toOption } map { _.toString stripPrefix "/" }
  }

  private case class Statistics(  // We don't care about synchronization
    var requestCount: Int = 0,
    var messageByteCount: Long = 0,
    var currentRequestIssuedAt: Option[Instant] = None,
    var failure: Option[Throwable] = None)

  private sealed trait TunnelState
  private case object Uninitialized extends TunnelState
  private case class RequestBeforeConnected(request: Connector.Request) extends TunnelState
  private case class ConnectedConnector(relais: ActorRef) extends TunnelState
}
