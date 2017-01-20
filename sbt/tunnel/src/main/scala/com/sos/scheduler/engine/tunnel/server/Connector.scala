package com.sos.scheduler.engine.tunnel.server

import akka.actor.SupervisorStrategy._
import akka.actor._
import akka.io.Tcp
import akka.util.ByteString
import com.sos.scheduler.engine.common.akkautils.Akkas.byteStringToTruncatedString
import com.sos.scheduler.engine.common.scalautil.{Logger, SetOnce}
import com.sos.scheduler.engine.common.tcp.MessageTcpBridge
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.timer.{Timer, TimerService}
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
private[server] final class Connector private(connectorHandler: ActorRef, tcp: ActorRef, connected: Tcp.Connected, timerService: TimerService)
extends Actor with FSM[State, Data] {
  import connected.remoteAddress
  import context.{dispatcher, parent, watch}

  private val tunnelIdOnce = new SetOnce[TunnelId]
  private val messageTcpBridge = context.actorOf(MessageTcpBridge.props(tcp, connected))
  private var messageTcpBridgeTerminated = false
  private var messageTcpBridgeCloseSent = false
  private var logger = Logger(getClass)

  private object inactivityWatchdog {
    private var timer: Timer[Unit] = null

    def stop(): Unit =
      if (timer != null) {
        timerService.cancel(timer)
        timer = null
      }

    def restart(timeoutOption: Option[Duration]): Unit = {
      stop()
      for (timeout ← timeoutOption) {
        val since = Instant.now()
        timer = timerService.delay(timeout, toString) onElapsed {
          val msg = BecameInactive(tunnelIdOnce(), since)
          logger.debug(s"$msg")
          connectorHandler ! msg
        }
      }
    }

    override def toString = s"${Connector.this} inactivityWatchdog"
  }

  override def supervisorStrategy = stoppingStrategy

  override def preStart() = {
    super.preStart()
    watch(messageTcpBridge)
    inactivityWatchdog.restart(Some(FirstRequestTimeout))
    startWith(ExpectingMessageFromTcp, NoData)
  }

  override def postStop() = {
    inactivityWatchdog.stop()
    super.postStop()
  }

  when(ExpectingMessageFromTcp) {
    case Event(MessageTcpBridge.MessageReceived(message), NoData) ⇒
      val connectionMessage = JsonParser(message.toArray[Byte]).convertTo[TunnelConnectionMessage]
      logger = Logger.withPrefix(getClass, connectionMessage.tunnelToken.id.toString)
      logger.trace(s"$connectionMessage")
      tunnelIdOnce := connectionMessage.tunnelToken.id
      parent ! AssociatedWithTunnelId(connectionMessage.tunnelToken, remoteAddress)
      goto(ExpectingRequest) using NoData

    case Event(MessageTcpBridge.MessageReceived(message), Respond(responsePromise)) ⇒
      responsePromise.success(message)
      goto(ExpectingRequest) using NoData

    case Event(MessageTcpBridge.PeerClosed, Respond(responsePromise)) ⇒
      responsePromise.failure(new TunnelConnectionClosedException(s"$toString: Peer has closed the connection while expecting a response"))
      stop()

    case Event(MessageTcpBridge.Failed(throwable), Respond(responsePromise)) ⇒
      responsePromise.failure(throwable)
      goto(ExpectingRequest) using NoData

    case Event(Close, Respond(responsePromise)) ⇒
      responsePromise.failure(new TunnelConnectionClosedException(s"$toString: Connection to peer has been closed by command"))
      closeBridge()
      goto(Closing) using NoData
  }

  when(ExpectingMessageFromTcp)(handleHeartbeat)

  when(ExpectingRequest) {
    case Event(Request(message, responsePromise, timeout), NoData) ⇒
      messageTcpBridge ! MessageTcpBridge.SendMessage(message)
      inactivityWatchdog.restart(timeout)
      goto(ExpectingMessageFromTcp) using Respond(responsePromise)

    case Event(MessageTcpBridge.PeerClosed, NoData) ⇒
      logger.trace("MessageTcpBridge.PeerClosed")
      stop()
  }

  when(ExpectingRequest)(handleHeartbeat)

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
      closeBridge()
      goto(Closing) using NoData

    case Event(closed: Tcp.ConnectionClosed, data) ⇒
      logger.debug(s"$closed")
      val exception = new TunnelConnectionClosedException(s"$toString: Connection has unexpectedly been closed: $closed")
      data match {
        case Respond(responsePromise) ⇒
          responsePromise.failure(exception)
          stop()
        case _ ⇒ stop(FSM.Failure(exception))
      }

    case Event(Terminated(_), _) ⇒
      messageTcpBridgeTerminated = true
      stop()

    case Event(unexpectedRequest: Request, _) ⇒
      val msg = s"Unexpected request in state $stateName"
      logger.error(s"$msg: $unexpectedRequest")
      unexpectedRequest.responsePromise tryFailure new IllegalStateException(msg)
      closeBridge()
      goto(Closing) using NoData

    case Event(event, NoData) ⇒
      val msg = s"Unexpected message received in state $stateName: $event"
      if (stateName == Closing) logger.debug(msg)
      else logger.error(msg)
      closeBridge()
      goto(Closing) using NoData
  }

  private def handleHeartbeat: StateFunction = {
    case Event(m @ Heartbeat(timeout: Duration), _) ⇒
      logger.debug(s"$m")
      inactivityWatchdog.restart(Some(timeout))
      stay()
  }

  onTransition {
    case _ -> Closing ⇒ inactivityWatchdog.stop()
  }

  private def closeBridge(): Unit =
    if (!messageTcpBridgeTerminated && !messageTcpBridgeCloseSent) {
      messageTcpBridgeCloseSent = true
      messageTcpBridge ! MessageTcpBridge.Close
    }

  override def toString = {
    val s = tunnelIdOnce toStringOr "tunnel"
    s"Connector($s server endpoint $remoteAddress)"
  }
}

private[server] object Connector {
  private val FirstRequestTimeout = 60.s

  private[server] def props(connectorHandler: ActorRef, tcp: ActorRef, connected: Tcp.Connected, timerService: TimerService) =
    Props { new Connector(connectorHandler, tcp, connected, timerService) }

  private[server] sealed trait Command
  private[server] case class Heartbeat(timeout: Duration) {
    override def toString = s"Heartbeat(timeout ${timeout.pretty})"
  }
  private[server] final case class Request(message: ByteString, responsePromise: Promise[ByteString], timeout: Option[Duration]) extends Command {
    override def toString = s"Request(${byteStringToTruncatedString(message)})"
  }
  private[server] case object Close extends Command

  private[server] sealed trait MyEvent
  private[server] final case class AssociatedWithTunnelId(tunnelToken: TunnelToken, peerAddress: InetSocketAddress) extends MyEvent
  private[server] final case class Response(message: ByteString) extends MyEvent {
    override def toString = s"Response(${byteStringToTruncatedString(message)})"
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
