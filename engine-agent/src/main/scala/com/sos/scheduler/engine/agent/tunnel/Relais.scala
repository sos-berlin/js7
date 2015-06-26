package com.sos.scheduler.engine.agent.tunnel

import akka.actor.{Actor, ActorRef, FSM}
import akka.io.Tcp
import akka.util.ByteString
import com.sos.scheduler.engine.agent.tunnel.LengthHeaderMessageCollector._
import com.sos.scheduler.engine.agent.tunnel.Relais._
import com.sos.scheduler.engine.common.scalautil.Logger
import java.net.InetSocketAddress
import org.scalactic.Requirements._
import scala.concurrent.Promise
import spray.json.JsonParser

/**
 * @author Joacim Zschimmer
 */
final class Relais(tcpConnection: ActorRef, peerAddress: InetSocketAddress) extends Actor with FSM[State, Data] {

  private var tunnelId: TunnelId = null
  private var tunnelEstablished = false

  tcpConnection ! Tcp.Register(self)
  startWith(CollectingResponseFromTcp, CollectingResponseFromTcpData(null))  // null !!!

  when(ExpectingRequest) {
    case Event(request: DirectedRequest, NoData) ⇒
      logger.trace(s"WaitingForRequest: Request(${request.message.size} bytes)")
      handleRequest(request)
  }

  private def handleRequest(request: DirectedRequest): FSM.State[Relais.State, Data] = {
    // TODO Große Nachrichten werden nicht auf einmal verschickt.
    tcpConnection ! Tcp.Write(intToBytesString(request.message.size) ++ request.message)
    goto(CollectingResponseFromTcp) using CollectingResponseFromTcpData(request.responsePromise)
  }

  when(CollectingResponseFromTcp) {
    case Event(Tcp.Received(bytes), CollectingResponseFromTcpData(responsePromise, messageBuilder)) ⇒
      logger.debug(s"CollectingResponseFromTcp: Received(${bytes.size} bytes)")
      require(sender() == tcpConnection)
      messageBuilder.apply(bytes) match {
        case None ⇒ stay()
        case Some(completeMessage) ⇒
          logger.trace(s"CollectingResponseFromTCP: Complete message has ${bytes.size} bytes")
          if (!tunnelEstablished) {
            logger.trace("Connection message")
            implicit val xxx = TunnelConnectionMessage.MyJsonFormat
            val m = JsonParser(completeMessage.toArray[Byte]).convertTo[TunnelConnectionMessage]
            tunnelId = m.tunnelId
            tunnelEstablished = true
            context.parent ! RelaisConnected(m.tunnelId)
          } else {
            responsePromise.success(completeMessage)
            //responsePromise.failure() ???
          }
          goto(ExpectingRequest) using NoData
      }
  }

  whenUnhandled {
    case Event(m @ Tcp.PeerClosed, _) ⇒
      logger.debug("$m")
      stop()

    case Event(Close, _) ⇒
      tcpConnection ! Tcp.Close
      stay()

    case Event(Tcp.Closed, _) ⇒
      stop()
  }

  override def toString = s"Release($tunnelId,$peerAddress)"
}

private[tunnel] object Relais {
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
}
