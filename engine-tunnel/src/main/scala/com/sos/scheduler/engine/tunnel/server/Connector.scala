package com.sos.scheduler.engine.tunnel.server

import akka.actor.SupervisorStrategy._
import akka.actor._
import akka.io.Tcp
import akka.util.ByteString
import com.sos.scheduler.engine.common.akkautils.Akkas.DummyCancellable
import com.sos.scheduler.engine.common.scalautil.{Logger, SetOnce}
import com.sos.scheduler.engine.common.tcp.MessageTcpBridge
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.tunnel.data.{TunnelConnectionMessage, TunnelId, TunnelToken}
import com.sos.scheduler.engine.tunnel.server.Connector._
import java.net.InetSocketAddress
import java.time.{Duration, Instant}
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
private[server] final class Connector private(connectorHandler: ActorRef, tcp: ActorRef, connected: Tcp.Connected)
extends Actor with FSM[State, Data] {
  import connected.remoteAddress
  import context.{dispatcher, parent, system, watch}

  private val tunnelIdOnce = new SetOnce[TunnelId]
  private val messageTcpBridge = context.actorOf(MessageTcpBridge.props(tcp, connected))
  private var messageTcpBridgeTerminated = false
  private var logger = Logger(getClass)

  private object inactivityWatchdog {
    private var timer: Cancellable = new DummyCancellable

    def stop() = timer.cancel()

    def restart(timeoutOption: Option[Duration]): Unit = {
      timer.cancel()
      for (timeout ← timeoutOption) {
        val since = Instant.now()
        timer = system.scheduler.scheduleOnce(timeout.toFiniteDuration) {
          val msg = BecameInactive(tunnelIdOnce(), since)
          logger.debug(s"$msg")
          connectorHandler ! msg
        }
      }
    }
  }

  override def supervisorStrategy = stoppingStrategy

  override def postStop() = inactivityWatchdog.stop()

  watch(messageTcpBridge)
  startWith(ExpectingMessageFromTcp, NoData)

  when(ExpectingMessageFromTcp) {
    case Event(MessageTcpBridge.MessageReceived(message), NoData) ⇒
      val connectionMessage = JsonParser(message.toArray[Byte]).convertTo(TunnelConnectionMessage.MyJsonFormat)
      logger = Logger.withPrefix(getClass, connectionMessage.tunnelToken.id.toString)
      logger.trace(s"$connectionMessage")
      tunnelIdOnce := connectionMessage.tunnelToken.id
      parent ! AssociatedWithTunnelId(connectionMessage.tunnelToken, remoteAddress)
      goto(ExpectingRequest) using NoData

    case Event(MessageTcpBridge.MessageReceived(message), Respond(responsePromise)) ⇒
      responsePromise.success(message)
      goto(ExpectingRequest) using NoData

    case Event(MessageTcpBridge.PeerClosed, Respond(responsePromise)) ⇒
      responsePromise.failure(new ConnectionClosedException(s"$toString: Peer has closed the connection while expecting a response"))
      stop()

    case Event(MessageTcpBridge.Failed(throwable), Respond(responsePromise)) ⇒
      responsePromise.failure(throwable)
      goto(ExpectingRequest) using NoData
  }

  when(ExpectingRequest) {
    case Event(m @ Request(message, responsePromise, timeout), NoData) ⇒
      messageTcpBridge ! MessageTcpBridge.SendMessage(message)
      inactivityWatchdog.restart(timeout)
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
    case Event(m @ Heartbeat(timeout: Duration), _) ⇒
      logger.debug(s"$m")
      inactivityWatchdog.restart(Some(timeout))
      stay()

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
      val exception = new ConnectionClosedException(s"$toString: Connection has unexpectedly been closed: $closed")
      data match {
        case Respond(responsePromise) ⇒
          responsePromise.failure(exception)
          stop()
        case _ ⇒ stop(FSM.Failure(exception))
      }

    case Event(Terminated(_), _) ⇒
      messageTcpBridgeTerminated = true
      stop()
  }

  onTransition {
    case _ -> Closing ⇒ inactivityWatchdog.stop()
  }

  override def toString = {
    val s = tunnelIdOnce toStringOr "tunnel"
    s"Connector($s server endpoint $remoteAddress)"
  }
}

private[server] object Connector {
  private[server] def props(connectorHandler: ActorRef, tcp: ActorRef, connected: Tcp.Connected) =
    Props { new Connector(connectorHandler, tcp, connected) }

  private[server] sealed trait Command
  private[server] case class Heartbeat(timeout: Duration)
  private[server] final case class Request(message: ByteString, responsePromise: Promise[ByteString], timeout: Option[Duration]) extends Command {
    override def toString = s"Request(${message.size} bytes)"
  }
  private[server] case object Close extends Command

  private[server] sealed trait MyEvent
  private[server] final case class AssociatedWithTunnelId(tunnelToken: TunnelToken, peerAddress: InetSocketAddress) extends MyEvent
  private[server] final case class Response(message: ByteString) extends MyEvent {
    override def toString = s"Response(${message.size} bytes)"
  }
  private[server] final case class BecameInactive(tunnelId: TunnelId, since: Instant) extends MyEvent

  private[server] sealed trait State
  private case object ExpectingRequest extends State
  private case object ExpectingMessageFromTcp extends State
  private case object Closing extends State

  private[server] sealed trait Data
  private case class Respond(responsePromise: Promise[ByteString]) extends Data
  private case object NoData extends Data
}
