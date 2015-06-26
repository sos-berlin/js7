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
//FIXME Increase heap  @RunWith(classOf[JUnitRunner])
final class TunnelTest extends FreeSpec {

  // TODO TunnelId extra salzen (mit base64)
  private val messageSizes = Iterator(MessageSizeMaximum, 0, 1) ++ Iterator.continually { nextRandomSize(10*1000*1000) }

  "Tunnel" in {
    val actorSystem = ActorSystem()
    import actorSystem.dispatcher
    val tunnelHandler = new TunnelHandler(actorSystem)
    val tunnelsAndServers = for (i ← 1 to TunnelCount) yield {
      val id = TunnelId(i.toString)
      val tunnel = tunnelHandler.newTunnel(id)
      val tcpServer = new TcpServer(tunnel.idWithPassword, tunnelHandler.localAddress)
      tcpServer.start()
      tunnel.connected onSuccess { case peerAddress: InetSocketAddress ⇒
        logger.info(s"$tunnel $peerAddress")
      }
      tunnel → tcpServer
    }
    for (_ ← 1 to 100) {
      val responseFutures = for ((tunnel, _) ← Random.shuffle(tunnelsAndServers)) yield {
        val request = ByteString.fromArray(new Array[Byte](messageSizes.next()))
        tunnel.sendRequest(request) map { response ⇒
          if (!byteStringsFastEqual(response, requestToResponse(request, tunnel.id))) fail("Response is not as expected")
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
  private val TunnelCount = 8
  private val MessageSizeMaximum = Relais.MessageSizeMaximum - 100 // requestToResponse adds some bytes
  private val logger = Logger(getClass)

  private def nextRandomSize(n: Int) = pow(2, log(n) / log(2) * Random.nextDouble()).toInt
  private def byteStringsFastEqual(a: ByteString, b: ByteString) = util.Arrays.equals(a.toArray[Byte], b.toArray[Byte])

  private def requestToResponse(request: ByteString, id: TunnelId): ByteString =
    request ++ ByteString.fromString(s" RESPONSE FROM $id")

  private class TcpServer(tunnelIdWithPassword: TunnelId.WithPassword, masterAddress: InetSocketAddress) extends Thread {
    val tunnelId = tunnelIdWithPassword.id
    setName(s"TCP Server $tunnelId")
    val terminatedPromise = Promise[Unit]()

    override def run(): Unit =
      try {
        val connection = new TcpConnection(masterAddress)
        connection.connect()
        connection.sendMessage(TunnelConnectionMessage(tunnelIdWithPassword).toByteString)
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
