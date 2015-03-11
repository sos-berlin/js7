package com.sos.scheduler.engine.taskserver

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp.Connect
import akka.io.{IO, Tcp}
import akka.util.ByteString
import com.sos.scheduler.engine.taskserver.TcpActor._
import java.net.InetSocketAddress

/**
 * @author Joacim Zschimmer
 */
private class TcpActor(address: InetSocketAddress, listener: ActorRef) extends Actor {

  import context.system

  override def preStart(): Unit = {
    IO(Tcp) ! Tcp.Connect(address, pullMode = true)
  }

  def receive = {
    case Tcp.CommandFailed(_: Connect) ⇒
      listener ! responses.Error
      context stop self

    case c @ Tcp.Connected(remote, local) =>
      //listener ! c
      val connection = sender()
      connection ! Tcp.Register(self)
      context become {
        case commands.Send(data) ⇒
          connection ! Tcp.Write(data)
        case commands.Close ⇒
          connection ! Tcp.Close
        case Tcp.Received(data) ⇒
          listener ! responses.Received(data)
        case Tcp.CommandFailed(w: Tcp.Write) ⇒
          // O/S buffer was full
          listener ! responses.Error
        case o: Tcp.ConnectionClosed ⇒
          // Tcp.PeerClosed, Tcp.ErrorClosed ⇒
          listener ! responses.Closed
          context stop self
      }
  }
}

private object TcpActor {
  object commands {
    case object Close
    final case class Send(message: ByteString)
  }

  object responses {
    case object Closed
    final case class Received(message: ByteString)
    case object Error
  }
}
