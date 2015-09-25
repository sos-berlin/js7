package com.sos.scheduler.engine.tunnel.server

import akka.actor.SupervisorStrategy.stoppingStrategy
import akka.actor.{Actor, ActorRef, Props, Terminated}
import akka.agent.Agent
import akka.io.{IO, Tcp}
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Collections.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.tunnel.data._
import com.sos.scheduler.engine.tunnel.server.ConnectorHandler._
import java.net.{InetAddress, InetSocketAddress}
import java.time.Instant
import java.time.Instant.now
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
 * @author Joacim Zschimmer
 */
private[tunnel] final class ConnectorHandler private extends Actor {

  import context.{actorOf, become, dispatcher, stop, system, watch}

  override def supervisorStrategy = stoppingStrategy

  private val register = mutable.Map[TunnelId, Entry]() withDefault { o ⇒ throw new NoSuchElementException(s"Unknown $o") }
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
      val name = {
        val peerInterface = connected.remoteAddress.getAddress.getHostAddress
        val peerPort = connected.remoteAddress.getPort
        s"Connector-TCP-$peerInterface:$peerPort"
      }
      val connector = actorOf(Connector.props(tcp = sender(), connected), name = name)
      watch(connector)

    case m @ NewTunnel(id, listener, startedByIpOption) ⇒
      logger.trace(s"$m")
      sender() ! Try[TunnelHandle] {
        val secret = TunnelToken.newSecret()
        val connectedPromise = Promise[InetSocketAddress]()
        val tunnel = new TunnelHandle(
          self,
          TunnelToken(id, secret),
          startedByIpOption = startedByIpOption,
          connectedPromise.future,
          peerAddress = () ⇒ connectedPromise.future.value map { _.get })
        register.insert(id → Entry(secret, tunnel, connectedPromise, listener))
        tunnel
      }

    case m @ Connector.AssociatedWithTunnelId(TunnelToken(id, secret), peerAddress) ⇒
      logger.trace(s"$m")
      val connector = sender()
      register.get(id) match {
        case None ⇒
          logger.error(s"Unknown TunnelId '$id' received from $connector")
          stop(connector)
        case Some(entry: Entry) ⇒
          if (secret != entry.secret) {
            logger.error(s"Tunnel secret from $connector does not match with $id")
            stop(connector)
          } else {
            entry.state match {
              case o: Entry.ConnectedConnector ⇒
                logger.error(s"${entry.state} has tried to connect twice with $id")
                stop(connector)
              case Entry.Uninitialized ⇒
              case Entry.RequestBeforeConnected(request) ⇒
                connector ! request
                entry.onRequestSent(request)
            }
          }
          entry.connectedPromise.success(peerAddress)
          entry.state = Entry.ConnectedConnector(connector)
          logger.debug(s"$id connected with ${entry.remoteAddressString}")
      }

    case m @ Terminated(child) ⇒
      for (id ← getTunnelIdByActor(child)) {
        removeEntry(id)
        logger.debug(s"$m, tunnel removed after Connector death")  // Should have been removed by CloseTunnel
      }

    case m @ DirectedRequest(tunnelToken, request) ⇒
      try {
        logger.trace(s"$m")
        val entry = checkedEntry(tunnelToken)
        entry.state match {
          case Entry.ConnectedConnector(connector) ⇒
            connector ! request
            entry.onRequestSent(request)
          case Entry.Uninitialized ⇒ entry.state = Entry.RequestBeforeConnected(request)
          case o: Entry.RequestBeforeConnected ⇒ sys.error("Second request before connection has been established")
        }
      } catch {
        case NonFatal(t) ⇒ request.responsePromise.failure(t)
      }

    case m @ CloseTunnel(tunnelToken) ⇒
      try {
        if (register contains tunnelToken.id) {
          checkedEntry(tunnelToken).state match {
            case Entry.ConnectedConnector(connector) ⇒ connector ! Connector.Close
            case _ ⇒
          }
          removeEntry(tunnelToken.id)
        }
      }
      catch {
        case NonFatal(t) ⇒ logger.error(s"$m: $t", t)
      }

    case GetOverview ⇒
      sender() ! TunnelHandlerOverview(
        tcpAddress = tcpAddressOption map { _.toString },
        tunnelCount = register.size)

    case GetTunnelOverviews ⇒
      sender() ! (register map { case (id, entry) ⇒
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

  private def removeEntry(id: TunnelId): Unit = register -= id

  private def checkedEntry(tunnelToken: TunnelToken) = {
    val entry = register(tunnelToken.id)
    if (tunnelToken.secret != entry.secret) throw new IllegalArgumentException(s"Wrong tunnel secret")
    entry
  }

  private def getTunnelIdByActor(actorRef: ActorRef): Option[TunnelId] =
    register.iterator collectFirst {
      case (id, entry) if PartialFunction.cond(entry.state) { case Entry.ConnectedConnector(connector) ⇒ connector == actorRef } ⇒ id }
}

private[tunnel] object ConnectorHandler {
  private val LocalInterface = "127.0.0.1"
  private val logger = Logger(getClass)

  private[tunnel] def props = Props { new ConnectorHandler }

  sealed trait Command

  private[tunnel] case object Start extends Command

  private[tunnel] case class NewTunnel[A <: TunnelListener](
    tunnelId: TunnelId,
    listener: Agent[A],
    startedByIpOption: Option[InetAddress])
  extends Command

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
    handle: TunnelHandle,
    connectedPromise: Promise[InetSocketAddress],
    listener: Agent[TunnelListener],
    var state: Entry.State = Entry.Uninitialized,
    statistics: Statistics = Statistics())
  {
    def remoteAddressString: String = remoteAddressStringOption getOrElse "(not connected via TCP)"

    def remoteAddressStringOption: Option[String] = connectedPromise.future.value flatMap { _.toOption } map { _.toString stripPrefix "/" }

    def onRequestSent(request: Connector.Request)(implicit ec:ExecutionContext): Unit = {
      statistics.updateWith(request)
      listener send { _.onRequest(request.message) }
    }
  }

  private object Entry {
    sealed trait State
    case object Uninitialized extends State
    case class RequestBeforeConnected(request: Connector.Request) extends State
    case class ConnectedConnector(connector: ActorRef) extends State
  }

  private case class Statistics(  // We don't care about synchronization ???
    var requestCount: Int = 0,
    var messageByteCount: Long = 0,
    var currentRequestIssuedAt: Option[Instant] = None,
    var failure: Option[Throwable] = None) {

    def updateWith(request: Connector.Request)(implicit ec: ExecutionContext): Unit = {
      currentRequestIssuedAt = Some(now)
      requestCount += 1
      messageByteCount += request.message.size
      request.responsePromise.future.onComplete { tried ⇒
        // We don't care about synchronization
        tried match {
          case Success(message) ⇒
            messageByteCount += message.size
            failure = None
          case Failure(t) ⇒
            failure = Some(t)
        }
        currentRequestIssuedAt = None
      }
    }
  }
}
