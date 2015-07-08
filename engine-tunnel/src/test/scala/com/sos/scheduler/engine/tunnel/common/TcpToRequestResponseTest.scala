package com.sos.scheduler.engine.tunnel.common

import akka.actor.ActorSystem
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.tcp.TcpConnection
import com.sos.scheduler.engine.tunnel.common.TcpToRequestResponseTest._
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
    autoClosing(TcpConnection.Listener.forLocalHostPort()) { listener ⇒
      val tcpToRequestResponse = new TcpToRequestResponse(actorSystem, connectTo = listener.boundAddress, executeRequest)
      tcpToRequestResponse.start()
      val tcpConnection = listener.accept()
      (tcpToRequestResponse, tcpConnection)
    }
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
