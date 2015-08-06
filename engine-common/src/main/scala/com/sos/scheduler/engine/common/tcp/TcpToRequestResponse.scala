package com.sos.scheduler.engine.common.tcp

import akka.actor.SupervisorStrategy._
import akka.actor._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.tcp.TcpToRequestResponse.{Close, Start, _}
import java.net.{InetSocketAddress, SocketAddress}
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal
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
  private val actor = actorSystem.actorOf(Props { new BridgeClientActor })
  private val startedPromise = Promise[Unit]()

  def start(): Unit = actor ! Start

  def close(): Unit = {
    startedPromise.future.onComplete { _ ⇒
      if (!actorClosed) {
        // Race condition: When actor has stopped itself just now, the message will be a dead letter
        actor ! Close
      } else {
        actorSystem.stop(actor)
      }
    }
  }

  private class BridgeClientActor extends Actor {
    import context.{actorOf, become, dispatcher, stop, system}

    private var bridge: ActorRef = null
    private var ownAddress: SocketAddress = null

    override def supervisorStrategy = stoppingStrategy

    def receive = expectingStart

    private def expectingStart: Receive = {
      case Start ⇒
        IO(Tcp) ! Tcp.Connect(connectTo)
        become(connecting)
    }

    private def connecting: Receive = {
      case connected: Tcp.Connected ⇒
        startedPromise.success(())
        ownAddress = connected.localAddress
        val tcp = sender()
        bridge = actorOf(Props { new MessageTcpBridge(tcp, connected)})
        context.watch(bridge)
        become(running)

      case m: Tcp.CommandFailed ⇒
        startedPromise.failure(new RuntimeException(m.toString))
    }

    private def running: Receive = {
      case MessageTcpBridge.MessageReceived(message) ⇒
        val future = try executeRequest(message) catch { case NonFatal(t) ⇒ Future.failed(t) }
        future onComplete {
          case Success(response) ⇒
            bridge ! MessageTcpBridge.SendMessage(response)
          case Failure(t) ⇒
            logger.error(s"$t", t)
            bridge ! MessageTcpBridge.Close // 2015-06-29 Tcp.Abort does not close the connection when peer is C++ JobScheduler
        }

      case Close ⇒
        if (bridge != null) {
          bridge ! MessageTcpBridge.Close
        } else {
          stop(self)
        }

      case Terminated(_) ⇒
        stop(self)
        actorClosed = true
    }
  }
}

object TcpToRequestResponse {
  private val logger = Logger(getClass)

  private case object Start
  private case object Close
}
