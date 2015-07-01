package com.sos.scheduler.engine.tunnel

import akka.actor.ActorSystem
import akka.util.ByteString
import com.sos.scheduler.engine.common.tcp.TcpConnection
import com.sos.scheduler.engine.tunnel.TcpToRequestResponseTest._
import java.net.InetSocketAddress
import java.nio.channels.ServerSocketChannel
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TcpToRequestResponseTest extends FreeSpec {

  private lazy val actorSystem = ActorSystem("TEST")

  "Some requests" in {
    val (tcpToRequestResponse, tcpConnection) = newTcpToRequestResponse()
    for (i ← 1 to 3) {
      val a = ByteString.fromString(s"TEST #$i")
      tcpConnection.sendMessage(a)
      assert(ByteString.fromByteBuffer(tcpConnection.receiveMessage().get) == requestToResponse(a))
    }
    tcpConnection.close()
    tcpToRequestResponse.close()
  }

  "On error while executing request the TCP connection is closed" in {
    val (tcpToRequestResponse, tcpConnection) = newTcpToRequestResponse()
    tcpConnection.sendMessage(Error)
    //intercept[IOException] { tcpConnection.receiveMessage() }
    tcpConnection.receiveMessage() shouldEqual None
    tcpConnection.close()
    tcpToRequestResponse.close()
  }

  "TcpToRequestResponse.close closes TCP connection" in {
    val (tcpToRequestResponse, tcpConnection) = newTcpToRequestResponse()
    tcpToRequestResponse.close()
    tcpConnection.receiveMessage() shouldEqual None
    tcpConnection.close()
  }

  private def newTcpToRequestResponse() = {
    val listener = ServerSocketChannel.open()
    listener.bind(new InetSocketAddress("127.0.0.1", 0))
    val serverAddress = listener.getLocalAddress.asInstanceOf[InetSocketAddress]
    val tcpToRequestResponse = new TcpToRequestResponse(actorSystem, remoteAddress = serverAddress, executeRequest)
    tcpToRequestResponse.start()
    val tcpConnection = new TcpConnection(listener.accept())
    (tcpToRequestResponse, tcpConnection)
  }
}

private object TcpToRequestResponseTest {
  private val Error = ByteString.fromString("ERROR")

  private def executeRequest(request: ByteString) =
    if (request == Error)
      Future.failed(new RuntimeException("TEST"))
    else
      Future.successful(requestToResponse(request))

  private def requestToResponse(o: ByteString): ByteString = o ++ ByteString(" RESPONSE")
}
