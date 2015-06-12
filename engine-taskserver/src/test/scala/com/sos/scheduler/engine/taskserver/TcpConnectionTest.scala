package com.sos.scheduler.engine.taskserver

import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.net.{InetAddress, InetSocketAddress, ServerSocket, Socket}
import java.nio.ByteBuffer
import java.util.concurrent.TimeoutException
import org.junit.runner.RunWith
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TcpConnectionTest extends FreeSpec with HasCloser with BeforeAndAfterAll {

  private val localhost = InetAddress.getByName("127.0.0.1")
  private lazy val listenSocket = new ServerSocket(0, 1, localhost).closeWithCloser
  private lazy val port = listenSocket.getLocalPort
  private lazy val tcpConnection = new TcpConnection(new InetSocketAddress(localhost, port))
  private var testSocket: Socket = null
  private def out = testSocket.getOutputStream
  private def in = testSocket.getInputStream

  override protected def afterAll() =
    try closer.close()
    finally super.afterAll()

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
    val myTcpConnection = new TcpConnection(new InetSocketAddress(localhost, port))
    connect(myTcpConnection)
    val future = Future { myTcpConnection.receiveMessage() }
    intercept[TimeoutException] { awaitResult(future, 500.ms) }
    myTcpConnection.close()
    intercept[java.nio.channels.AsynchronousCloseException] { awaitResult(future, 1.s) }
  }

  private def connect(tcpConnection: TcpConnection): Unit = {
    val connected = Future { tcpConnection.connect() }
    listenSocket.setSoTimeout(10*1000)
    testSocket = listenSocket.accept().closeWithCloser
    awaitResult(connected, 1.s)
  }

  private def smallIntToBytes(i: Int) = Array[Byte](0.toByte, 0.toByte, (i >> 8).toByte, (i & 0xFF).toByte)
}
