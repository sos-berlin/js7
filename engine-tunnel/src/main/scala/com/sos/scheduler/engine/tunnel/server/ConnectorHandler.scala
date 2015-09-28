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

  private val register = mutable.Map[TunnelId, Handle]() withDefault { id ⇒ throw new NoSuchElementException(s"Unknown $id") }
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
                connector ! request
                handle.onRequestSent(request)
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
            handle.onRequestSent(request)
          case Handle.Uninitialized ⇒ handle.state = Handle.RequestBeforeConnected(request)
          case o: Handle.RequestBeforeConnected ⇒ sys.error("Second request before connection has been established")
        }
      } catch {
        case NonFatal(t) ⇒ request.responsePromise.failure(t)
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
      }
      catch {
        case NonFatal(t) ⇒ logger.error(s"$m: $t", t)
      }

    case GetOverview ⇒
      sender() ! TunnelHandlerOverview(
        tcpAddress = tcpAddressOption map { _.toString },
        tunnelCount = register.size)

    case GetTunnelOverviews ⇒
      sender() ! (register map { case (id, handle) ⇒
        TunnelOverview(
          id,
          handle.startedByHttpIpOption,
          handle.remoteAddressStringOption,
          TunnelStatistics(
            requestCount = handle.statistics.requestCount,
            messageByteCount = handle.statistics.messageByteCount,
            currentRequestIssuedAt = handle.statistics.currentRequestIssuedAt,
            handle.statistics.failure map { _.toString }))
      }).toVector
  }

  private def removeHandle(id: TunnelId): Unit = register -= id

  private def checkedHandle(tunnelToken: TunnelToken) = {
    val handle = register(tunnelToken.id)
    if (tunnelToken.secret != handle.tunnelToken.secret) throw new IllegalArgumentException(s"Wrong tunnel secret")
    handle
  }

  private def getTunnelIdByActor(actorRef: ActorRef): Option[TunnelId] =
    register.iterator collectFirst {
      case (id, handle) if PartialFunction.cond(handle.state) { case Handle.ConnectedConnector(connector) ⇒ connector == actorRef } ⇒ id }
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

  private class Handle(
    connectorHandler: ActorRef,
    val tunnelToken: TunnelToken,
    val startedByHttpIpOption: Option[InetAddress],
    val connectedPromise: Promise[InetSocketAddress],
    val listener: Agent[TunnelListener],
    var state: Handle.State = Handle.Uninitialized,
    val statistics: Statistics = Statistics())
  extends TunnelHandle {

    def close(): Unit = connectorHandler ! ConnectorHandler.CloseTunnel(tunnelToken)

    override def toString = s"TunnelHandler($id,HTTP client ${startedByHttpIpOption getOrElse "unknown"} -> TCP server ${serverAddressOption getOrElse "not yet connected"})"

    private def serverAddressOption: Option[InetSocketAddress] = connectedPromise.future.value map { _.get }

    def connected = connectedPromise.future

    def remoteAddressString: String = remoteAddressStringOption getOrElse "(not connected via TCP)"

    def remoteAddressStringOption: Option[String] = connectedPromise.future.value flatMap { _.toOption } map { _.toString stripPrefix "/" }

    def onRequestSent(request: Connector.Request)(implicit ec:ExecutionContext): Unit = {
      statistics.updateWith(request)
      listener send { _.onRequest(request.message) }
    }
  }

  private object Handle {
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
