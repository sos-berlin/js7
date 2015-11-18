package com.sos.scheduler.engine.common.tcp

import akka.actor.SupervisorStrategy._
import akka.actor._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Futures.catchInFuture
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.tcp.TcpToRequestResponse.{Close, Start, _}
import java.net.InetSocketAddress
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

/**
 * Adapter between prefix-length framed TCP requests, a request executor and prefix-length framed TCP responses.
 *
 * @author Joacim Zschimmer
 */
final class TcpToRequestResponse(
  actorSystem: ActorSystem,
  connectTo: InetSocketAddress,
  executeRequest: ByteString ⇒ Future[ByteString])
extends AutoCloseable {

  import actorSystem.dispatcher

  @volatile private var actorClosed = false
  private val actor = actorSystem.actorOf(Props { new MessageTcpBridgeClient })
  private val startedPromise = Promise[Unit]()

  def start(connectionMessage: Option[ByteString] = None): Unit =
    actor ! Start(connectionMessage)

  def close(): Unit =
    startedPromise.future.onComplete { _ ⇒
      if (!actorClosed) {
        // Race condition: When actor has stopped itself just now, the message will be a dead letter
        actor ! Close
      } else {
        actorSystem.stop(actor)
      }
    }

  private class MessageTcpBridgeClient extends Actor {
    import context.{actorOf, become, dispatcher, stop, system}

    override def supervisorStrategy = stoppingStrategy

    def receive = expectingStart

    private def expectingStart: Receive = {
      case Start(connectionMessage) ⇒
        IO(Tcp) ! Tcp.Connect(connectTo)
        become(connecting(connectionMessage))
    }

    private def connecting(connectionMessageOption: Option[ByteString]): Receive = {
      case connected: Tcp.Connected ⇒
        startedPromise.success(())
        val tcp = sender()
        val bridge = actorOf(MessageTcpBridge.props(tcp, connected), name = "MessageTcpBridge")
        context.watch(bridge)
        for (m ← connectionMessageOption) bridge ! MessageTcpBridge.SendMessage(m)
        become(running(bridge = bridge))
      case m: Tcp.CommandFailed ⇒
        startedPromise.failure(new RuntimeException(m.toString))
      case Close ⇒
        stop(self)
    }

    private def running(bridge: ActorRef): Receive = {
      case MessageTcpBridge.MessageReceived(message) ⇒
        catchInFuture { executeRequest(message) } onComplete {
          case Success(response) ⇒
            bridge ! MessageTcpBridge.SendMessage(response)
          case Failure(t) ⇒
            logger.error(s"$t", t)
            bridge ! MessageTcpBridge.Close // 2015-06-29 Tcp.Abort does not close the connection when peer is C++ JobScheduler
        }
      case Close ⇒
        bridge ! MessageTcpBridge.Close
      case Terminated(_) ⇒
        stop(self)
        actorClosed = true
    }
  }
}

object TcpToRequestResponse {
  private val logger = Logger(getClass)

  private case class Start(connectionMessage: Option[ByteString])
  private case object Close
}
