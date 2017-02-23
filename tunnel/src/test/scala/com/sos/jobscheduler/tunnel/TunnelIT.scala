package com.sos.scheduler.engine.tunnel

import akka.actor.ActorSystem
import akka.agent.Agent
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.tcp.{MessageTcpBridge, BlockingTcpConnection}
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.Stopwatch
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.tunnel.TunnelIT._
import com.sos.scheduler.engine.tunnel.data.{TunnelConnectionMessage, TunnelId, TunnelToken}
import com.sos.scheduler.engine.tunnel.server.{TunnelListener, TunnelServer}
import java.net.InetSocketAddress
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.{Future, Promise, blocking}
import scala.math.{log, min, pow}
import scala.util.Random

/**
 *
 * JS-1386 HTTP tunnel replaces RPC TCP connections
 *
 * @author Joacim Zschimmer
 */
final class TunnelIT extends FreeSpec with BeforeAndAfterAll {

  private lazy val actorSystem = ActorSystem(getClass.getSimpleName)
  import actorSystem.dispatcher
  private implicit val timerService = TimerService(idleTimeout = Some(1.s))
  private lazy val tunnelServer = new TunnelServer(actorSystem)

  override protected def afterAll() = {
    actorSystem.shutdown()
    super.afterAll()
  }

  "Simple" in {
    val tunnel = tunnelServer.newTunnel(TunnelId("TEST-TUNNEL"), Agent(TunnelListener.StopListening))
    val tcpServer = new TcpServer(tunnel.tunnelToken, tunnelServer.proxyAddress)
    tcpServer.start()
    for (i ← 1 to 3) {
      val request = ByteString(s"TEST-REQUEST #$i")
      val responded = tunnel.request(request, timeout = None) map { response ⇒
        assert(response == requestToResponse(request, tunnel.id))
      }
      awaitResult(responded, 10.s)
    }
    assert(!tcpServer.terminatedPromise.isCompleted)
    tunnel.close()
    awaitResult(tcpServer.terminatedPromise.future, 10.s)
  }

  s"$TunnelCount tunnels with random sizes" in {
    val messageSizes = /*Iterator(MessageSizeMaximum, 0, 1) ++*/ Iterator.continually { nextRandomSize(1000*1000) }
    val tunnelsAndServers = for (i ← 1 to TunnelCount) yield {
      val id = TunnelId(i.toString)
      val tunnel = tunnelServer.newTunnel(id, Agent(TunnelListener.StopListening))
      val tcpServer = new TcpServer(tunnel.tunnelToken, tunnelServer.proxyAddress)
      tcpServer.start()
      for (peerAddress: InetSocketAddress ← tunnel.connected) logger.info(s"$tunnel $peerAddress")
      tunnel → tcpServer
    }
    val tunnelRuns = for ((tunnel, _) ← tunnelsAndServers) yield Future {
      blocking {
        val stepSize = min(Iterations, 1000)
        for (i ← 0 until Iterations by stepSize) {
          val m = Stopwatch.measureTime(stepSize, "requests") {
            val request = ByteString.fromArray(Array.fill[Byte](messageSizes.next())(i.toByte))
            val responded = tunnel.request(request, timeout = None) map { response ⇒
              if (!byteStringsFastEqual(response, requestToResponse(request, tunnel.id))) fail("Response is not as expected")
            }
            awaitResult(responded, 10.s)
          }
          logger.info(s"$tunnel: $i requests, $m")
        }
      }
    }
    awaitResult(Future.sequence(tunnelRuns), 1.h)
    for ((tunnel, tcpServer) ← tunnelsAndServers) {
      tunnel.close()
      awaitResult(tcpServer.terminatedPromise.future, 10.s)
    }
    tunnelServer.close()
  }

  "tunnelServer.close" in {
    tunnelServer.close()
  }
}

object TunnelIT {
  private val TunnelCount = 4
  private val Iterations = 100
  private val MessageSizeMaximum = MessageTcpBridge.MessageSizeMaximum - 100 // requestToResponse adds some bytes
  private val logger = Logger(getClass)

  private def nextRandomSize(n: Int) = pow(2, log(n) / log(2) * Random.nextDouble()).toInt
  private def byteStringsFastEqual(a: ByteString, b: ByteString) = java.util.Arrays.equals(a.toArray[Byte], b.toArray[Byte])

  private def requestToResponse(request: ByteString, id: TunnelId): ByteString =
    request ++ ByteString(s" RESPONSE FROM $id")

  private class TcpServer(tunnelToken: TunnelToken, masterAddress: InetSocketAddress) extends Thread {
    // Somewhat unusual, the server connects the client and sends a TunnelConnectionMessage, before acting as a usual server.
    val tunnelId = tunnelToken.id
    setName(s"TCP Server $tunnelId")
    val terminatedPromise = Promise[Unit]()

    override def run(): Unit =
      try {
        val connection = BlockingTcpConnection.connect(masterAddress)
        connection.sendMessage(TunnelConnectionMessage(tunnelToken).toByteString)
        for (request ← (Iterator.continually { connection.receiveMessage() } takeWhile { _.nonEmpty }).flatten) {
          val response = requestToResponse(request, tunnelId)
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
