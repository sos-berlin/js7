package com.sos.scheduler.engine.tunnel

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.tunnel.LengthHeaderMessageCollector._
import com.sos.scheduler.engine.tunnel.MessageTcpBridge._

/**
 * Full-duplex bridge between length-prefixed framed messages and a TCP stream.
 * Messages from message side are framed and forwarded to TCP.
 * The bytes stream from TCP is unframed and the messages are forwared to the message side.
 *
 * @note The used LengthHeaderMessageCollector is not yet full duplex.
 *
 * @author Joacim Zschimmer
 */
final class MessageTcpBridge(tcp: ActorRef, connected: Tcp.Connected)
extends Actor {

  import connected.{localAddress, remoteAddress}
  import context.{become, parent, stop}

  private val logger = Logger.withPrefix(getClass, s"$localAddress-$remoteAddress")
  private val messageBuilder: LengthHeaderMessageCollector = new LengthHeaderMessageCollector

  tcp ! Tcp.Register(self)

  def receive = running

  private def running: Receive = {
    case m @ SendMessage(message) ⇒
      logger.trace(s"$m")
      if (message.size > MessageSizeMaximum) {
        parent ! Failed(newTooBigException(message.size))
      } else {
        tcp ! Tcp.Write(intToBytesString(message.size) ++ message)
      }

    case Close ⇒
      tcp ! Tcp.Close
      become(closing)

    case Abort ⇒
      tcp ! Tcp.Abort
      become(closing)

    case Tcp.Received(bytes) ⇒
      val completedMessageOption = messageBuilder.apply(bytes)
      messageBuilder.expectedLength match {
        case Some(len) if len > MessageSizeMaximum ⇒
          parent ! Failed(newTooBigException(len))
          tcp ! Tcp.Abort
          become(closing)
        case _ ⇒
          for (completeMessage ← completedMessageOption) {
            val m = MessageReceived(completeMessage)
            logger.trace(s"$m")
            parent ! m
          }
      }

    case m @ Tcp.PeerClosed ⇒
      logger.debug(s"$m")
      parent ! PeerClosed
      stop(self)

    case closed: Tcp.ConnectionClosed ⇒
      logger.debug(s"$closed")
      val exception = new RuntimeException(s"Connection with $remoteAddress has unexpectedly been closed: $closed")
      parent ! Failed(exception)
      stop(self)
  }

  private def closing: Receive = {
    case closed: Tcp.ConnectionClosed ⇒
      logger.debug(s"$closed")
      stop(self)
  }

  override def toString = s"MessageTcpBridge($localAddress-$remoteAddress)"
}

private[tunnel] object MessageTcpBridge {
  val MessageSizeMaximum = 100*1000*1000

  sealed trait Command

  final case class SendMessage(message: ByteString) extends Command {
    override def toString = s"SendMessage(${message.size} bytes)"
  }

  case object Close extends Command
  case object Abort extends Command

  sealed trait Event
  case object PeerClosed extends Event

  final case class MessageReceived(message: ByteString) extends Event {
    override def toString = s"MessageReceived(${message.size} bytes)"
  }

  final case class Failed(throwable: Throwable) extends Event

  private def newTooBigException(size: Int) =
    new IllegalArgumentException(s"Message ($size bytes) is bigger than the possible maximum of $MessageSizeMaximum bytes")
}
