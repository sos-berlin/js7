package com.sos.scheduler.engine.taskserver

import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.utils.FreeTcpPortFinder.findRandomFreeTcpPort
import com.sos.scheduler.engine.test.scalatest.HasCloserBeforeAndAfterAll
import java.net.{InetAddress, InetSocketAddress, ServerSocket, Socket, SocketException}
import java.nio.ByteBuffer
import java.util.concurrent.TimeoutException
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TcpConnectionTest extends FreeSpec with HasCloserBeforeAndAfterAll {

  private val ipAddress = InetAddress.getByName("127.0.0.1")
  private lazy val port = findRandomFreeTcpPort()
  private lazy val tcpConnection = new TcpConnection(new InetSocketAddress(ipAddress, port))
  private var testSocket: Socket = null
  private lazy val listenSocket = new ServerSocket(port, 1, ipAddress).closeWithCloser
  private def out = testSocket.getOutputStream
  private def in = testSocket.getInputStream

  "connect" in {
    connect(tcpConnection)
  }

  "receiveMessage" in {
    val length = 260
    val data = Array.fill(length) { Random.nextInt().toByte }
    out.write(smallIntToBytes(length) ++ data)
    tcpConnection.receiveMessage() shouldEqual Some(ByteBuffer.wrap(data))
  }

  "sendMessage" in {
    val length = 261
    val sentData = Array.fill(length) { Random.nextInt().toByte }
    tcpConnection.sendMessage(sentData, length)
    val receivedData = new Array[Byte](4 + length)
    val receivedLength = in.read(receivedData)
    receivedLength shouldEqual 4 + length
    receivedData.toVector shouldEqual smallIntToBytes(length) ++ sentData.toVector
  }

  "Close connection" in {
    testSocket.shutdownOutput()
    tcpConnection.receiveMessage() shouldEqual None
    tcpConnection.close()
    in.read() shouldEqual -1
    testSocket.close()
  }

  "Close while receiving" in {
    val myTcpConnection = new TcpConnection(new InetSocketAddress(ipAddress, port))
    connect(myTcpConnection)
    val future = Future { myTcpConnection.receiveMessage() }
    intercept[TimeoutException] { Await.result(future, 500.millis) }
    myTcpConnection.close()
    intercept[SocketException] { Await.result(future, 1.seconds) }.getMessage should include ("closed")  // Message is JVM specific
  }

  private def connect(tcpConnection: TcpConnection): Unit = {
    val connected = Future { tcpConnection.connect() }
    listenSocket.setSoTimeout(10*1000)
    testSocket = listenSocket.accept().closeWithCloser
    Await.result(connected, 1.seconds)
  }

  private def smallIntToBytes(i: Int) = Array[Byte](0.toByte, 0.toByte, (i >> 8).toByte, (i & 0xFF).toByte)
}
