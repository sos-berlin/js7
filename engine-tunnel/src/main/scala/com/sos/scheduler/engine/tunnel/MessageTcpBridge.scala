package com.sos.scheduler.engine.tunnel

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.tunnel.LengthHeaderMessageCollector._
import com.sos.scheduler.engine.tunnel.MessageTcpBridge._
import java.net.InetSocketAddress

/**
 * Full-duplex bridge between length-prefixed framed messages and a TCP stream.
 * Messages from message side are framed and forwarded to TCP.
 * The bytes stream from TCP is unframed and the messages are forwared to the message side.
 *
 * @note The used LengthHeaderMessageCollector is not yet full duplex.
 *
 * @author Joacim Zschimmer
 */
private[tunnel] final class MessageTcpBridge(tcp: ActorRef, peerAddress: InetSocketAddress)
extends Actor {

  import context.{parent, stop}

  private val messageBuilder: LengthHeaderMessageCollector = new LengthHeaderMessageCollector

  tcp ! Tcp.Register(self)

  def receive = running

  def running: Receive = {
    case m @ SendMessage(message) ⇒ //if sender() == parent ⇒
      logger.trace(s"$m")
      if (message.size > MessageSizeMaximum) {
        parent ! Failed(newTooBigException(message.size))
      } else {
        tcp ! Tcp.Write(intToBytesString(message.size) ++ message)
      }

    case Close ⇒
      tcp ! Tcp.Close
      context.become(closing)

    case Abort ⇒
      tcp ! Tcp.Abort
      context.become(closing)

    case Tcp.Received(bytes) if sender() == tcp ⇒
      logger.trace(s"Received (${bytes.size} bytes)")
      val completedMessageOption = messageBuilder.apply(bytes)
      messageBuilder.expectedLength match {
        case Some(len) if len > MessageSizeMaximum ⇒
          parent ! Failed(newTooBigException(len))
          tcp ! Tcp.Abort
          context.become(closing)
        case _ ⇒
          for (completeMessage ← completedMessageOption) {
            val m = MessageReceived(completeMessage)
            logger.trace(s"$m")
            parent ! m
          }
      }

    case closed: Tcp.ConnectionClosed ⇒
      logger.debug(s"$closed")
      val exception = new RuntimeException(s"Connection with $peerAddress has unexpectedly been closed: $closed")
      parent ! Failed(exception)
      stop(self)
  }

  private def closing: Receive = {
    case closed: Tcp.ConnectionClosed ⇒
      logger.debug(s"$closed")
      stop(self)
  }

  override def toString = s"Relais2($peerAddress)"
}

private[tunnel] object MessageTcpBridge {
  private[tunnel] val MessageSizeMaximum = 100*1000*1000
  private val logger = Logger(getClass)

  private[tunnel] case object Close
  private[tunnel] case object Abort

  private[tunnel] final case class SendMessage(message: ByteString) {
    override def toString = s"SendMessage(${message.size} bytes)"
  }

  private[tunnel] final case class MessageReceived(message: ByteString) {
    override def toString = s"MessageReceived(${message.size} bytes)"
  }

  private[tunnel] final case class Failed(throwable: Throwable)

  private def newTooBigException(size: Int) =
    new IllegalArgumentException(s"Message ($size bytes) is bigger than the possible maximum of $MessageSizeMaximum bytes")
}
