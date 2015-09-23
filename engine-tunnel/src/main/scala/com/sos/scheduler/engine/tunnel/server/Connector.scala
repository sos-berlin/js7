package com.sos.scheduler.engine.tunnel.server

import akka.actor.SupervisorStrategy._
import akka.actor._
import akka.io.Tcp
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.tcp.MessageTcpBridge
import com.sos.scheduler.engine.tunnel.data.{TunnelConnectionMessage, TunnelId, TunnelToken}
import com.sos.scheduler.engine.tunnel.server.Connector._
import java.net.InetSocketAddress
import scala.concurrent.Promise
import spray.json.JsonParser

/**
 * Establishes and handles a connection between a request/response site (client) and a TCP site (server).
 * <p>
 * In the JobScheduler Agent, the request/response client is a HTTP web service,
 * and the TCP site is the TaskServer.
 * <p>
 * The Agent creates this actor when a task is started.
 * The actor then waits for the first message from the TCP site, a [[TunnelConnectionMessage]].
 * This messages denotes the TunnelId of the tunnel, the task wishes to connect to.
 * <p>
 * After the TunnelId is known, the actor expects a `Request` and forwards it the TCP site (length-prefix framed),
 * Then, the actor awaits a framed TCP message and forwards it as a `Response` to the original requester.
 * This is repeated for every request.
 *
 * @author Joacim Zschimmer
 */
private[server] final class Connector private(tcp: ActorRef, connected: Tcp.Connected)
extends Actor with FSM[State, Data] {
  import connected.remoteAddress

  private var tunnelId: TunnelId = null
  private val messageTcpBridge = context.actorOf(Props { new MessageTcpBridge(tcp, connected) })
  private var messageTcpBridgeTerminated = false

  override def supervisorStrategy = stoppingStrategy

  context.watch(messageTcpBridge)
  startWith(ExpectingMessageFromTcp, NoData)

  when(ExpectingMessageFromTcp) {
    case Event(MessageTcpBridge.MessageReceived(message), NoData) ⇒
      val connectionMessage = JsonParser(message.toArray[Byte]).convertTo(TunnelConnectionMessage.MyJsonFormat)
      logger.trace(s"$connectionMessage")
      tunnelId = connectionMessage.tunnelToken.id
      context.parent ! AssociatedWithTunnelId(connectionMessage.tunnelToken, remoteAddress)
      goto(ExpectingRequest) using NoData

    case Event(MessageTcpBridge.MessageReceived(message), Respond(responsePromise)) ⇒
      responsePromise.success(message)
      goto(ExpectingRequest) using NoData

    case Event(MessageTcpBridge.PeerClosed, Respond(responsePromise)) ⇒
      val e = new RuntimeException(s"Peer $remoteAddress has closed the connection while expecting a response")
      responsePromise.failure(e)
      stop(FSM.Failure(e))

    case Event(MessageTcpBridge.Failed(throwable), Respond(responsePromise)) ⇒
      responsePromise.failure(throwable)
      goto(ExpectingRequest) using NoData
  }

  when(ExpectingRequest) {
    case Event(m @ Request(message, responsePromise), NoData) ⇒
      messageTcpBridge ! MessageTcpBridge.SendMessage(message)
      goto(ExpectingMessageFromTcp) using Respond(responsePromise)

    case Event(m @ MessageTcpBridge.PeerClosed, NoData) ⇒
      logger.trace(s"$m")
      stop()
  }

  when(Closing) {
    case Event(closed: Tcp.ConnectionClosed, _) ⇒
      logger.debug(s"$closed")
      stop()
  }

  whenUnhandled {
    case Event(m @ MessageTcpBridge.Failed(throwable), _) ⇒
      logger.warn(s"$m", throwable)
      if (!messageTcpBridgeTerminated) {
        messageTcpBridge ! MessageTcpBridge.Abort
      }
      goto(Closing) using NoData

    case Event(Close, _) ⇒
      if (!messageTcpBridgeTerminated) {
        messageTcpBridge ! MessageTcpBridge.Close
      }
      goto(Closing) using NoData

    case Event(closed: Tcp.ConnectionClosed, data) ⇒
      logger.debug(s"$closed")
      val exception = new RuntimeException(s"Connection with $remoteAddress has unexpectedly been closed: $closed")
      data match {
        case Respond(responsePromise) ⇒ responsePromise.failure(exception)
        case _ ⇒
      }
      stop(FSM.Failure(exception))

    case Event(Terminated(_), _) ⇒
      messageTcpBridgeTerminated = true
      stop()
  }

  override def toString = s"Connector($tunnelIdString)"

  private def tunnelIdString = if (tunnelId == null) "tunnel is not yet established" else tunnelId.string
}

private[server] object Connector {
  private val logger = Logger(getClass)

  def props(tcp: ActorRef, connected: Tcp.Connected) = Props { new Connector(tcp, connected) }


  // Actor messages commanding this actor

  private[server] final case class Request(message: ByteString, responsePromise: Promise[ByteString]) {
    override def toString = s"Request(${message.size} bytes)"
  }

  private[server] case object Close


  // Event messages from this actor

  private[server] final case class AssociatedWithTunnelId(tunnelToken: TunnelToken, peerAddress: InetSocketAddress)

  private[server] final case class Response(message: ByteString) {
    override def toString = s"Response(${message.size} bytes)"
  }

  private[server] sealed trait State
  private case object ExpectingRequest extends State {}
  private case object ExpectingMessageFromTcp extends State
  private case object Closing extends State

  private[server] sealed trait Data
  private case class Respond(responsePromise: Promise[ByteString]) extends Data
  private case object NoData extends Data
}
