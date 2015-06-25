package com.sos.scheduler.engine.tunnel

import akka.actor.{Actor, ActorRef, FSM}
import akka.io.Tcp
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.tunnel.LengthHeaderMessageCollector._
import com.sos.scheduler.engine.tunnel.Relais._
import java.net.InetSocketAddress
import scala.concurrent.Promise
import scala.util.control.NonFatal
import spray.json.JsonParser

/**
 * @author Joacim Zschimmer
 */
private[tunnel] final class Relais(tcpConnection: ActorRef, peerAddress: InetSocketAddress)
extends Actor with FSM[State, Data] {

  private var tunnelId: TunnelId = null

  tcpConnection ! Tcp.Register(self)
  startWith(CollectingResponseFromTcp, CollectingResponseFromTcp.ForConnectionMessage())

  when(ExpectingRequest) {
    case Event(m @ Request(message, responsePromise), NoData) ⇒
      logger.trace(s"ExpectingRequest: Got $m")
      try {
        if (message.size > MessageSizeMaximum) throw newTooBigException(message.size)
        tcpConnection ! Tcp.Write(intToBytesString(message.size) ++ message)
        goto(CollectingResponseFromTcp) using CollectingResponseFromTcp.ForResponse(responsePromise)
      } catch {
        case NonFatal(t) ⇒
          responsePromise.failure(t)
          stay()
      }
  }

  when(CollectingResponseFromTcp) {
    case Event(Tcp.Received(bytes), CollectingResponseFromTcp.ForConnectionMessage(messageBuilder)) ⇒
      collect(bytes, messageBuilder) { completeMessage ⇒
        val connectionMessage = JsonParser(completeMessage.toArray[Byte]).convertTo(TunnelConnectionMessage.MyJsonFormat)
        logger.trace(s"Connection message: $connectionMessage")
        tunnelId = connectionMessage.tunnelIdWithPassword.id
        context.parent ! RelaisAssociatedWithTunnelId(connectionMessage.tunnelIdWithPassword, peerAddress)
          goto(ExpectingRequest) using NoData
      }

    case Event(Tcp.Received(bytes), CollectingResponseFromTcp.ForResponse(responsePromise, messageBuilder)) ⇒
      try
        collect(bytes, messageBuilder) { completeMessage ⇒
          responsePromise.success(completeMessage)
          goto(ExpectingRequest) using NoData
        }
      catch {
        case NonFatal(t) ⇒
          responsePromise.failure(t)
          stop(FSM.Failure(t))
      }
  }

  private def collect(bytes: ByteString, messageBuilder: LengthHeaderMessageCollector)(onComplete: ByteString => State): State = {
    logger.trace(s"CollectingResponseFromTCP: Received ${ bytes.size } bytes)")
    for (len ← messageBuilder.expectedLength if len > MessageSizeMaximum) throw newTooBigException(len)
    messageBuilder.apply(bytes) match {
      case None ⇒ stay()
      case Some(completeMessage) ⇒
        logger.trace(s"CollectingResponseFromTCP: Complete message has ${completeMessage.size} bytes")
        onComplete(completeMessage)
    }
  }

  when(Closing) {
    case Event(Closing, _) ⇒
      stop()
    case Event(closed @ Tcp.Closed, _) ⇒
      logger.debug(s"$closed")
      stop()
  }

  whenUnhandled {
    case Event(Close, _) ⇒
      tcpConnection ! Tcp.Close
      goto(Closing) using NoData
    case Event(closed: Tcp.ConnectionClosed, data) ⇒
      logger.debug(s"$closed")
      val exception = new RuntimeException(s"Connection with $peerAddress has unexpectedly been closed: $closed")
      data match {
        case CollectingResponseFromTcp.ForResponse(responsePromise, _) ⇒ responsePromise.failure(exception)
        case _ ⇒
      }
      stop(FSM.Failure(exception))
  }

  override def toString = s"Relais($tunnelIdString)"

  private def tunnelIdString = if (tunnelId == null) "tunnel is not yet established" else tunnelId.string
}

private[tunnel] object Relais {
  private[tunnel] val MessageSizeMaximum = 100*1000*1000
  private val logger = Logger(getClass)

  private[tunnel] sealed trait State
  private[tunnel] sealed trait Data

  private case object ExpectingRequest extends State {}

  private case object CollectingResponseFromTcp extends State {
    final case class ForResponse(
      responsePromise: Promise[ByteString],
      messageBuilder: LengthHeaderMessageCollector = new LengthHeaderMessageCollector)
    extends Data

    case class ForConnectionMessage(
      messageBuilder: LengthHeaderMessageCollector = new LengthHeaderMessageCollector)
    extends Data
  }

  private case object Closing extends State

  private case object NoData extends Data


  private[tunnel] final case class RelaisAssociatedWithTunnelId(tunnelIdWithPassword: TunnelId.WithPassword, peerAddress: InetSocketAddress)

  private[tunnel] case object Close

  private def newTooBigException(size: Int) =
    new IllegalArgumentException(s"Message ($size bytes) is bigger than the possible maximum of $MessageSizeMaximum bytes")
}
