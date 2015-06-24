package com.sos.scheduler.engine.agent.tunnel

import akka.actor.ActorSystem
import akka.util.ByteString
import com.sos.scheduler.engine.agent.tunnel.TunnelTest._
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.taskserver.TcpConnection
import java.net.InetSocketAddress
import java.util
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.concurrent.{Future, Promise}
import scala.math.{log, pow}
import scala.util.Random

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TunnelTest extends FreeSpec {

  // TODO Sehr lange Nachrichten
  // TODO TunnelId extra salzen (mit base64)
  // TODO Fehlerbehandlung: TunnelHandler stabil; HTTP-Request in allen Fehlerfällen beantworten (es gibt kein Timeout)
  private val sizeGenerator = Iterator(MessageSizeMaximum, 0, 1) ++ Iterator.continually { nextRandomSize(MessageSizeMaximum) }

  "Tunnel" in {
    val actorSystem = ActorSystem()
    import actorSystem.dispatcher
    val tunnelHandler = new TunnelHandler(actorSystem)
    val tunnelsAndServers = for (i ← 1 to TunnelCount) yield {
      val id = TunnelId(i.toString)
      tunnelHandler.newTunnel(id) → new TcpServer(id, tunnelHandler.tcpAddress)
    }
    for ((_, tcpServer) ← tunnelsAndServers) tcpServer.start()
    for (_ ← 1 to 3) {
      val responseFutures = for ((tunnel, _) ← Random.shuffle(tunnelsAndServers)) yield {
        val request = ByteString.fromArray(new Array[Byte](sizeGenerator.next()))
        tunnel.sendRequest(request) map { response ⇒
          if (!byteStringsFastEqual(response, requestToResponse(request, tunnel.id))) fail ("Response is not as expected")
        }
      }
      awaitResult(Future.sequence(responseFutures), 600.s)
    }
    for ((tunnel, tcpServer) ← tunnelsAndServers) {
      tunnel.close()
      awaitResult(tcpServer.terminatedPromise.future, 10.s)
    }
    tunnelHandler.close()
  }
}

object TunnelTest {
  private val TunnelCount = 1
  private val MessageSizeMaximum = Relais.MessageSizeMaximum - 100 // requestToResponse adds some bytes
  private val logger = Logger(getClass)

  private def nextRandomSize(n: Int) = pow(2, log(n) / log(2) * Random.nextDouble()).toInt
  private def byteStringsFastEqual(a: ByteString, b: ByteString) = util.Arrays.equals(a.toArray[Byte], b.toArray[Byte])

  private def requestToResponse(request: ByteString, id: TunnelId): ByteString =
    request ++ ByteString.fromString(s" RESPONSE FROM $id")

  private class TcpServer(tunnelId: TunnelId, masterAddress: InetSocketAddress) extends Thread {
    setName(s"TCP Server $tunnelId")
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
}
