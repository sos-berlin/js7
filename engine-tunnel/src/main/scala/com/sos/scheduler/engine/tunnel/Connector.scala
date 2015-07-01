package com.sos.scheduler.engine.tunnel

import akka.actor.{Actor, ActorRef, FSM, Props}
import akka.io.Tcp
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.tunnel.Connector._
import com.sos.scheduler.engine.tunnel.data.{TunnelId, TunnelToken}
import java.net.InetSocketAddress
import scala.concurrent.Promise
import spray.json.JsonParser

/**
 * Establish and handles a tunnel connection between a request/response site and TCP site.
 * <p>
 * In the JobScheduler Agent, the request/response client is a HTTP web service,
 * and the TCP site is the TaskServer.
 * <p>
 * The Agent creates this actor when a task is started.
 * The actor then waits for the first message from the TCP site, a [[TunnelConnectionMessage]].
 * This messages denotes the TunnelId of the tunnel, the process wishes to connect to.
 * <p>
 * After the TunnelId is known, the actor expeteds a `Request` and forwards it the TCP site (length-prefix framed),
 * Then, the actor awaits a framed TCP message and forwards it as a `Response` to the orignal requestor.
 *
 * @author Joacim Zschimmer
 */
private[tunnel] final class Connector(tcp: ActorRef, peerAddress: InetSocketAddress)
extends Actor with FSM[State, Data] {

  private var tunnelId: TunnelId = null

  val messageTcpBridge = context.actorOf(Props { new MessageTcpBridge(tcp, peerAddress)})

  startWith(ExpectingMessageFromTcp, NoData)

  when(ExpectingMessageFromTcp) {
    case Event(MessageTcpBridge.MessageReceived(message), NoData) ⇒
      val connectionMessage = JsonParser(message.toArray[Byte]).convertTo(TunnelConnectionMessage.MyJsonFormat)
      logger.trace(s"$connectionMessage")
      tunnelId = connectionMessage.tunnelToken.id
      context.parent ! ConnectorAssociatedWithTunnelId(connectionMessage.tunnelToken, peerAddress)
      goto(ExpectingRequest) using NoData

    case Event(MessageTcpBridge.MessageReceived(message), Respond(responsePromise)) ⇒
      responsePromise.success(message)
      goto(ExpectingRequest) using NoData

    case Event(MessageTcpBridge.Failed(throwable), Respond(responsePromise)) ⇒
      responsePromise.failure(throwable)
      goto(ExpectingRequest) using NoData
  }

  when(ExpectingRequest) {
    case Event(m @ Request(message, responsePromise), NoData) ⇒
      messageTcpBridge ! MessageTcpBridge.SendMessage(message)
      goto(ExpectingMessageFromTcp) using Respond(responsePromise)
  }

  when(Closing) {
    case Event(Closing, _) ⇒
      stop()
    case Event(closed @ Tcp.Closed, _) ⇒
      logger.debug(s"$closed")
      stop()
  }

  whenUnhandled {
    case Event(m @ MessageTcpBridge.Failed(throwable), _) ⇒
      logger.warn(s"$m", throwable)
      messageTcpBridge ! MessageTcpBridge.Abort
      goto(Closing) using NoData
    case Event(Close, _) ⇒
      messageTcpBridge ! MessageTcpBridge.Close
      goto(Closing) using NoData
    case Event(closed: Tcp.ConnectionClosed, data) ⇒
      logger.debug(s"$closed")
      val exception = new RuntimeException(s"Connection with $peerAddress has unexpectedly been closed: $closed")
      data match {
        case Respond(responsePromise) ⇒ responsePromise.failure(exception)
        case _ ⇒
      }
      stop(FSM.Failure(exception))
  }

  override def toString = s"Connector($tunnelIdString)"

  private def tunnelIdString = if (tunnelId == null) "tunnel is not yet established" else tunnelId.string
}

private[tunnel] object Connector {
  private val logger = Logger(getClass)


  // Actor messages commanding this actor

  private[tunnel] final case class Request(message: ByteString, responsePromise: Promise[ByteString]) {
    override def toString = s"Request(${message.size} bytes)"
  }

  private[tunnel] case object Close


  // Event messages from this actor

  private[tunnel] final case class ConnectorAssociatedWithTunnelId(tunnelToken: TunnelToken, peerAddress: InetSocketAddress)

  private[tunnel] final case class Response(message: ByteString) {
    override def toString = s"Response(${message.size} bytes)"
  }

  private[tunnel] sealed trait State
  private case object ExpectingRequest extends State {}
  private case object ExpectingMessageFromTcp extends State
  private case object Closing extends State

  private[tunnel] sealed trait Data
  final case class Respond(responsePromise: Promise[ByteString]) extends Data
  private case object NoData extends Data
}
