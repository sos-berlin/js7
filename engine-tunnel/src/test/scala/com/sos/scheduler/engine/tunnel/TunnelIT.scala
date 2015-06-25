package com.sos.scheduler.engine.tunnel

import akka.actor.ActorSystem
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.tcp.TcpConnection
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.Stopwatch
import com.sos.scheduler.engine.tunnel.TunnelIT._
import java.net.InetSocketAddress
import java.util
import org.scalatest.FreeSpec
import scala.concurrent.{Future, Promise}
import scala.math.{log, min, pow}
import scala.util.Random

/**
 * @author Joacim Zschimmer
 */
//FIXME Increase heap  @RunWith(classOf[JUnitRunner])
final class TunnelIT extends FreeSpec {

  private val messageSizes = Iterator(MessageSizeMaximum, 0, 1) ++ Iterator.continually { nextRandomSize(1000*1000) }

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
    val tunnelRuns = for ((tunnel, _) ← tunnelsAndServers) yield Future {
      val stepSize = min(Iterations, 1000)
      for (i ← 0 until Iterations by stepSize) {
        val m = Stopwatch.measureTime(stepSize, "request") {
          val request = ByteString.fromArray(Array.fill[Byte](messageSizes.next())(i.toByte))
          val responded = tunnel.sendRequest(request) map { response ⇒
            if (!byteStringsFastEqual(response, requestToResponse(request, tunnel.id))) fail("Response is not as expected")
          }
          awaitResult(responded, 10.s)
        }
        logger.info(s"${tunnel.id}: $i requests, $m")
      }
    }
    awaitResult(Future.sequence(tunnelRuns), 1.h)
    for ((tunnel, tcpServer) ← tunnelsAndServers) {
      tunnel.close()
      awaitResult(tcpServer.terminatedPromise.future, 10.s)
    }
    tunnelHandler.close()
  }
}

object TunnelIT {
  private val TunnelCount = 4
  private val Iterations = 100
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
