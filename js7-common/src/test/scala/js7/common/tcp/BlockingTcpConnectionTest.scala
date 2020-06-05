package js7.common.tcp

import akka.util.ByteString
import java.net.{InetAddress, InetSocketAddress, ServerSocket, Socket}
import java.util.concurrent.TimeoutException
import js7.base.time.ScalaTime._
import js7.base.utils.Closer.syntax._
import js7.base.utils.HasCloser
import js7.common.scalautil.Futures._
import js7.common.scalautil.Futures.implicits._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import scala.util.Random

/**
 * @author Joacim Zschimmer
 */
final class BlockingTcpConnectionTest extends AnyFreeSpec with HasCloser with BeforeAndAfterAll {

  private val localhost = InetAddress.getByName("127.0.0.1")
  private var listenSocket: ServerSocket = _
  private var tcpConnection: BlockingTcpConnection =  _
  private var testSocket: Socket = null
  private def out = testSocket.getOutputStream
  private def in = testSocket.getInputStream

  override protected def afterAll() =
    try closer.close()
    finally super.afterAll()

  "connect" in {
    listenSocket = new ServerSocket(0, 1, localhost).closeWithCloser
    tcpConnection = connect()
  }

  "ownPort" in {
    assert(tcpConnection.ownPort > 0)
    assert(tcpConnection.ownPort != listenSocket.getLocalPort)
  }

  "isConnected" in {
    assert(tcpConnection.isConnected)
  }

  "receiveMessage" in {
    val length = 0x012233
    val data = Array.fill(length) { Random.nextInt().toByte }
    val message = smallIntToBytes(length) ++ data
    for (_ <- 1 to 10) {
      val received = namedThreadFuture("receiveMessage") { tcpConnection.receiveMessage() }
      out.write(message)
      assert((received await 10.s) == Some(ByteString(data)))
    }
  }

  "sendMessage" in {
    val length = 261
    val sentData = Array.fill(length) { Random.nextInt().toByte }
    val received = namedThreadFuture("receive") {
      val receivedData = new Array[Byte](4 + length)
      val receivedLength = in.read(receivedData)
      (receivedData, receivedLength)
    }
    tcpConnection.sendMessage(ByteString.fromArray(sentData))
    val (receivedData, receivedLength) = received await 10.s
    receivedLength shouldEqual 4 + length
    receivedData.toVector shouldEqual smallIntToBytes(length) ++ sentData.toVector
  }

  "Close connection" in {
    testSocket.shutdownOutput()
    tcpConnection.receiveMessage() shouldEqual None
    tcpConnection.close()
    assert(!tcpConnection.isConnected)
    in.read() shouldEqual -1
    testSocket.close()
  }

  "Close while receiving" in {
    val myTcpConnection = connect()
    val future = blockingThreadFuture { myTcpConnection.receiveMessage() }
    intercept[TimeoutException] { awaitResult(future, 500.ms) }
    myTcpConnection.close()
    intercept[java.nio.channels.AsynchronousCloseException] { awaitResult(future, 1.s) }
  }

  private def connect(): BlockingTcpConnection = {
    val connected = blockingThreadFuture { BlockingTcpConnection.connect(new InetSocketAddress(localhost, listenSocket.getLocalPort)) }
    listenSocket.setSoTimeout(10*1000)
    testSocket = listenSocket.accept().closeWithCloser
    awaitResult(connected, 1.s)
  }

  private def smallIntToBytes(i: Int) = Array[Byte](0.toByte, (i >> 16).toByte, (i >> 8).toByte, (i & 0xFF).toByte)
}
