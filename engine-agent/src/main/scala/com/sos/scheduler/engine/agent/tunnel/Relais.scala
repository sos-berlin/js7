package com.sos.scheduler.engine.agent.tunnel

import akka.actor.{Actor, ActorRef, FSM}
import akka.io.Tcp
import akka.util.ByteString
import com.sos.scheduler.engine.agent.tunnel.LengthHeaderMessageCollector._
import com.sos.scheduler.engine.agent.tunnel.Relais._
import com.sos.scheduler.engine.common.scalautil.Logger
import java.net.InetSocketAddress
import scala.concurrent.Promise
import scala.util.control.NonFatal
import spray.json.JsonParser

/**
 * @author Joacim Zschimmer
 */
final class Relais(tcpConnection: ActorRef, peerAddress: InetSocketAddress)
extends Actor with FSM[State, Data] {

  private var tunnelId: TunnelId = null

  tcpConnection ! Tcp.Register(self)
  startWith(CollectingResponseFromTcp, CollectingResponseFromTcpData(null))  // null !!!

  when(ExpectingRequest) {
    case Event(request: DirectedRequest, NoData) ⇒
      logger.trace(s"ExpectingRequest: Got ${request.message.size} bytes")
      try {
        if (request.message.size > MessageSizeMaximum) throw newTooBigException(request.message.size)
        tcpConnection ! Tcp.Write(intToBytesString(request.message.size) ++ request.message)
        goto(CollectingResponseFromTcp) using CollectingResponseFromTcpData(request.responsePromise)
      } catch {
        case NonFatal(t) ⇒
          request.responsePromise.failure(t)
          stay()
      }
  }

  when(CollectingResponseFromTcp) {
    case Event(Tcp.Received(bytes), CollectingResponseFromTcpData(responsePromise, messageBuilder)) ⇒
      try {
        logger.trace(s"CollectingResponseFromTCP: Received ${bytes.size} bytes)")
        for (len ← messageBuilder.expectedLength if len > MessageSizeMaximum) throw newTooBigException(len)
        messageBuilder.apply(bytes) match {
          case None ⇒ stay()
          case Some(completeMessage) ⇒
            logger.trace(s"CollectingResponseFromTCP: Complete message has ${completeMessage.size} bytes")
            if (tunnelId == null) {
              establishTunnel(completeMessage)
            } else {
              responsePromise.success(completeMessage)
            }
            goto(ExpectingRequest) using NoData
        }
      } catch {
        case NonFatal(t) ⇒
          responsePromise.failure(t)
          stop(FSM.Failure(t))
      }
  }

  private def establishTunnel(connectionMessage: ByteString): Unit = {
    logger.trace("Connection message")
    val m = JsonParser(connectionMessage.toArray[Byte]).convertTo(TunnelConnectionMessage.MyJsonFormat)
    tunnelId = m.tunnelId
    context.parent ! RelaisConnected(m.tunnelId)
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
        case CollectingResponseFromTcpData(responsePromise, _) ⇒ responsePromise.failure(exception)
        case _ ⇒
      }
      stop(FSM.Failure(exception))
  }

  override def toString = s"Relais(${Option(tunnelId) getOrElse "tunnel not yet established"},$peerAddress)"
}

private[tunnel] object Relais {
  private[tunnel] val MessageSizeMaximum = 100*1000*1000
  private val logger = Logger(getClass)

  private[tunnel] sealed trait State
  private[tunnel] case object ExpectingRequest extends State
  private[tunnel] case object CollectingResponseFromTcp extends State
  private[tunnel] case object Closing extends State

  private[tunnel] sealed trait Data
  private case object NoData extends Data
  private[tunnel] case class CollectingResponseFromTcpData(
    responsePromise: Promise[ByteString],
    messageBuilder: LengthHeaderMessageCollector = new LengthHeaderMessageCollector)
  extends Data

  private[tunnel] case class DirectedRequest(tunnelId: TunnelId, message: ByteString, responsePromise: Promise[ByteString])

  private[tunnel] final case class RelaisConnected(tunnelId: TunnelId)

  private[tunnel] case object Close

  private def newTooBigException(size: Int) =
    new IllegalArgumentException(s"Message ($size bytes) is bigger than the possible maximum of $MessageSizeMaximum bytes")
}
