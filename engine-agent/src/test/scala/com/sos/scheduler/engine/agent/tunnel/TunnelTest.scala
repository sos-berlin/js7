package com.sos.scheduler.engine.agent.tunnel

import akka.actor.ActorSystem
import akka.util.ByteString
import com.sos.scheduler.engine.agent.tunnel.TunnelTest._
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.taskserver.TcpConnection
import java.net.InetSocketAddress
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.concurrent.{Future, Promise}
import scala.util.Random

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TunnelTest extends FreeSpec {

  // TODO Sehr lange Nachrichten
  // TODO TunnelId extra salzen (mit base64)
  // TODO Fehlerbehandlung: TunnelHandler stabil; HTTP-Request in allen Fehlerfällen beantworten (es gibt kein Timeout)

  "Tunnel" in {
    val actorSystem = ActorSystem()
    import actorSystem.dispatcher
    val tunnelHandler = new TunnelHandler(actorSystem)
    val tunnelsAndServers = for (i ← 1 to 3) yield {
      val id = TunnelId(i.toString)
      tunnelHandler.newTunnel(id) → new TcpServer(id, tunnelHandler.tcpAddress)
    }
    for ((_, tcpServer) ← tunnelsAndServers) tcpServer.start()
    for (_ ← 1 to 3) {
      val responseFutures = for ((tunnel, _) ← Random.shuffle(tunnelsAndServers)) yield {
        val request = ByteString.fromString(Random.nextString(10))
        tunnel.sendRequest(request) map { response ⇒ assert(response == requestToResponse(request, tunnel.tunnelId)) }
      }
      awaitResult(Future.sequence(responseFutures), 10.s)
    }
    for ((tunnel, tcpServer) ← tunnelsAndServers) {
      tunnel.close()
      awaitResult(tcpServer.terminatedPromise.future, 10.s)
    }
    tunnelHandler.close()
  }
}

object TunnelTest {
  private val logger = Logger(getClass)

  private class TcpServer(tunnelId: TunnelId, masterAddress: InetSocketAddress) extends Thread {
    setName("TCP Server $tunnelId")
    val terminatedPromise = Promise[Unit]()

    override def run(): Unit =
      try {
        val connection = new TcpConnection(masterAddress)
        connection.connect()
        connection.sendMessage(TunnelConnectionMessage(tunnelId).toByteString)
        for (request ← (Iterator.continually { connection.receiveMessage() } takeWhile { _.nonEmpty }).flatten) {
          val response = requestToResponse(ByteString.fromByteBuffer(request), tunnelId)
          logger.debug(s"$tunnelId")
          connection.sendMessage(response)
        }
        connection.close()
        terminatedPromise.success(())
      }
      catch {
        case t: Throwable ⇒ terminatedPromise.failure(t)
      }
  }

  private def requestToResponse(request: ByteString, id: TunnelId): ByteString =
    request ++ ByteString.fromString(s" RESPONSE FROM $id")
}
