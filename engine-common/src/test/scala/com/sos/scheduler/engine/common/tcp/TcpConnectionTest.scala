package com.sos.scheduler.engine.common.tcp

import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.net.{InetAddress, InetSocketAddress, ServerSocket, Socket}
import java.util.concurrent.TimeoutException
import org.junit.runner.RunWith
import org.scalatest.Matchers._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TcpConnectionTest extends FreeSpec with HasCloser with BeforeAndAfterAll with ScalaFutures {

  private val localhost = InetAddress.getByName("127.0.0.1")
  private var listenSocket: ServerSocket = _
  private var port: Int = _
  private var tcpConnection: TcpConnection =  _
  private var testSocket: Socket = null
  private def out = testSocket.getOutputStream
  private def in = testSocket.getInputStream

  override protected def afterAll() =
    try closer.close()
    finally super.afterAll()

  "connect" in {
    listenSocket = new ServerSocket(0, 1, localhost).closeWithCloser
    port = listenSocket.getLocalPort
    tcpConnection = connect()
  }

  "receiveMessage" in {
    val length = 0x012233
    val data = Array.fill(length) { Random.nextInt().toByte }
    val message = smallIntToBytes(length) ++ data
    for (_ ← 1 to 10) {
      val received = Future { tcpConnection.receiveMessage() }
      out.write(message)
      assert(received.futureValue == Some(ByteString(data)))
    }
  }

  "sendMessage" in {
    val length = 261
    val sentData = Array.fill(length) { Random.nextInt().toByte }
    val received = Future {
      val receivedData = new Array[Byte](4 + length)
      val receivedLength = in.read(receivedData)
      (receivedData, receivedLength)
    }
    tcpConnection.sendMessage(ByteString.fromArray(sentData))
    val (receivedData, receivedLength) = received.futureValue
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
    val myTcpConnection = connect()
    val future = Future { myTcpConnection.receiveMessage() }
    intercept[TimeoutException] { awaitResult(future, 500.ms) }
    myTcpConnection.close()
    intercept[java.nio.channels.AsynchronousCloseException] { awaitResult(future, 1.s) }
  }

  private def connect(): TcpConnection = {
    val connected = Future { TcpConnection.connect(new InetSocketAddress(localhost, port)) }
    listenSocket.setSoTimeout(10*1000)
    testSocket = listenSocket.accept().closeWithCloser
    awaitResult(connected, 1.s)
  }

  private def smallIntToBytes(i: Int) = Array[Byte](0.toByte, (i >> 16).toByte, (i >> 8).toByte, (i & 0xFF).toByte)
}
