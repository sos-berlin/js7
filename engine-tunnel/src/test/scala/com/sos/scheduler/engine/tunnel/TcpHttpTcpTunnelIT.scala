package com.sos.scheduler.engine.tunnel

import akka.actor.{ActorSystem, Props}
import akka.agent.Agent
import akka.io.{IO, Tcp}
import akka.util.{ByteString, Timeout}
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.tcp.TcpConnection
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.Stopwatch.measureTime
import com.sos.scheduler.engine.common.time.alarm.AlarmClock
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.scheduler.engine.http.server.heartbeat.HeartbeatService
import com.sos.scheduler.engine.tunnel.TcpHttpTcpTunnelIT._
import com.sos.scheduler.engine.tunnel.client.{TcpToHttpBridge, WebTunnelClient}
import com.sos.scheduler.engine.tunnel.data.{TunnelConnectionMessage, TunnelId, TunnelToken}
import com.sos.scheduler.engine.tunnel.server.{TunnelAccess, TunnelListener, TunnelServer}
import com.sos.scheduler.engine.tunnel.web.TunnelWebServices._
import java.net.InetSocketAddress
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.Random
import spray.can.Http
import spray.http.Uri
import spray.http.Uri.Path
import spray.routing.HttpServiceActor

/**
 * Tests a whole tunnel from TCP via HTTP to TCP.
 *
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TcpHttpTcpTunnelIT extends FreeSpec {

  private implicit val timeout = Timeout(5.seconds)
  private implicit val alarmClock = new AlarmClock(10.ms, idleTimeout = Some(1.s))

  "Normal application" in {
    val (clientSide, serverSide) = startTunneledSystem()
    val n = 3
    for (i ← 1 to n) clientSide.checkSendReceive()
    clientSide.tcp.sendMessage(TerminateMessage)
    clientSide.requireEOF()
    serverSide.awaitTermination()
    serverSide.closeTunnel()
    clientSide.closeBrigde()
    val listenedMessages = Await.result(serverSide.listener.future(), 1.second).messages
    assert(listenedMessages.size == n + 1)
    assert(listenedMessages(n) == TerminateMessage)
  }

  "Server-side tunnel is closed" in {
    val (clientSide, serverSide) = startTunneledSystem()
    clientSide.checkSendReceive()
    serverSide.closeTunnel()  // JobScheduler closes the AgentProcess
    serverSide.awaitTermination()
    clientSide.tcp.sendMessage(ByteString("AFTER CLOSED"))
    clientSide.requireEOF()
    clientSide.closeBrigde()
  }

  "TCP server closes connection and terminates" in {
    val (clientSide, serverSide) = startTunneledSystem()
    clientSide.checkSendReceive()
    serverSide.connection.close()
    serverSide.awaitTermination()
    clientSide.tcp.sendMessage(ByteString("AFTER CLOSED"))
    clientSide.tcp.receiveMessage() shouldEqual None
    serverSide.closeTunnel()
    clientSide.closeBrigde()
  }

  "Speed test" in {
    val (clientSide, serverSide) = startTunneledSystem()
    measureTime(1000, "requests") {
      clientSide.checkSendReceive()
    }
    serverSide.closeTunnel()
    clientSide.closeBrigde()
  }
}

object TcpHttpTcpTunnelIT {
  private val TerminateMessage = ByteString("TERMINATE")
  private val logger = Logger(getClass)

  private def startTunneledSystem()(implicit alarmClock: AlarmClock) = {
    val serverSideTunnelHandler = new ServerSideTunnelHandler
    val server = new TunnelledTcpServer(serverSideTunnelHandler)
    val client = new ClientSide(serverSideTunnelHandler.uri, server.tunnelToken)
    (client, server)
  }

  private class ClientSide(uri: Uri, tunnelToken: TunnelToken) {
    private implicit val actorSystem = ActorSystem(getClass.getSimpleName)
    private val clientSideListener = TcpConnection.Listener.forLocalHostPort()
    private val tcpHttpBridge = new TcpToHttpBridge(
      actorSystem,
      clientSideListener.boundAddress,
      tunnelToken,
      new WebTunnelClient(
        ClientSide.this.tunnelToken,
        tunnelUri = {
          val uri = ClientSide.this.uri withPath Path("/test/tunnel")
          uri withPath (uri.path / tunnelToken.id.string)
        },
        heartbeatRequestorOption = None))
    tcpHttpBridge.start()
    val tcp = clientSideListener.accept()

    def checkSendReceive(): Unit = {
      val request = ByteString(s"TEST " + Random.nextInt())
      val response = sendReceive(request)
      assert(response contains requestToResponse(tunnelToken.id, request))
    }

    def sendReceive(request: ByteString): Option[ByteString] = {
      tcp.sendMessage(request)
      tcp.receiveMessage()
    }

    def requireEOF(): Unit = tcp.receiveMessage() shouldEqual None

    def closeBrigde() = tcpHttpBridge.close()
  }

  private class TunnelledTcpServer(handler: ServerSideTunnelHandler) {
    private val tunnel = handler.newTunnel(TunnelId("TEST-TUNNEL"))
    private val tcpServer = new TcpServer(tunnel.tunnelToken, handler.tcpAddress)
    def listener = handler.listener
    tcpServer.start()

    def connection = tcpServer.connection
    def awaitTermination() = Await.ready(tcpServer.terminatedPromise.future, 10.s.toConcurrent)
    def tunnelToken = tunnel.tunnelToken
    def closeTunnel() = tunnel.close()
  }

  private class ServerSideTunnelHandler(implicit alarmClock: AlarmClock) {
    val actorSystem = ActorSystem(getClass.getSimpleName)
    val tunnelServer = new TunnelServer(actorSystem)
    val uri = startWebServer()
    import actorSystem.dispatcher
    val listener = Agent(new TestTunnelListener)

    def newTunnel(id: TunnelId) = tunnelServer.newTunnel(id, listener)
    def tcpAddress = tunnelServer.proxyAddress

    private def startWebServer(): Uri = {
      val startedPromise = Promise[InetSocketAddress]()
      implicit val alarmClock = new AlarmClock(100.ms, idleTimeout = Some(1.s))
      val heartbeatService = new HeartbeatService
      actorSystem.actorOf(Props { new TestWebServiceActor(findRandomFreeTcpPort(), startedPromise, tunnelServer.tunnelAccess, heartbeatService) })
      val httpAddress = awaitResult(startedPromise.future, 10.s)
      Uri(s"http://${httpAddress.getAddress.getHostAddress}:${httpAddress.getPort}")
    }
  }

  private class TestWebServiceActor(
    port: Int,
    startedPromise: Promise[InetSocketAddress],
    tunnelAccess: TunnelToken ⇒ TunnelAccess,
    heartbeatService: HeartbeatService)
  extends HttpServiceActor {

    IO(Http)(context.system) ! Http.Bind(self, interface = "127.0.0.1", port = port)

    def receive = {
      case Http.Bound(address) ⇒
        startedPromise.success(address)
        context.become(running)

      case m @ Tcp.CommandFailed(_: Http.Bind) ⇒
        logger.error(m.toString)
        context.stop(self)
    }

    def running: Receive = runRoute {
      (decompressRequest() & compressResponseIfRequested(())) {
        (post & pathPrefix("test" / "tunnel" / Segment)) { idString ⇒
          tunnelRequestRoute(TunnelId(idString))(
            tunnelAccess,
            onHeartbeat = (_, _) ⇒ {})
        }
      }
    }
  }

  private class TestTunnelListener(val messages: Vector[ByteString] = Vector()) extends TunnelListener {
    def onRequest(msg: ByteString) = new TestTunnelListener(messages :+ msg)
  }

  private class TcpServer(tunnelToken: TunnelToken, masterAddress: InetSocketAddress) extends Thread {
    // Somewhat unusual, the server connects the client and sends a TunnelConnectionMessage, before acting as a usual server.
    val tunnelId = tunnelToken.id
    setName(s"TCP Server $tunnelId")
    val terminatedPromise = Promise[Unit]()
    val connection = TcpConnection.connect(masterAddress)

    override def run(): Unit =
      try {
        connection.sendMessage(TunnelConnectionMessage(tunnelToken).toByteString)
        for (request ← Iterator.continually { connection.receiveMessage().get } takeWhile { _ != TerminateMessage }) {
          connection.sendMessage(requestToResponse(tunnelId, request))
        }
        connection.close()
        terminatedPromise.success(())
      }
      catch {
        case t: Throwable ⇒ terminatedPromise.failure(t)
      }
  }

  private def requestToResponse(id: TunnelId, request: ByteString): ByteString = request ++ ByteString(s" RESPONSE FROM $id")
}
