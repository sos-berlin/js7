package com.sos.scheduler.engine.common.tcp

import akka.actor.ActorSystem
import akka.util.ByteString
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.tcp.TcpToRequestResponseTest._
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class TcpToRequestResponseTest extends FreeSpec with BeforeAndAfterAll {

  private lazy val actorSystem = ActorSystem(getClass.getSimpleName)

  override protected def afterAll() = {
    actorSystem.shutdown()
    super.afterAll()
  }

  for ((testName, connectionMessageOption) ← List("without connection message" → None, "with connection message" → Some(ByteString("Connection message")))) {
    s"Some requests, $testName" in {
      val (tcpToRequestResponse, tcpConnection) = newTcpToRequestResponse(connectionMessageOption)
      for (m ← connectionMessageOption) {
        assert(tcpConnection.receiveMessage().get == m)
      }
      for (i ← 1 to 3) {
        val a = ByteString(s"TEST #$i")
        tcpConnection.sendMessage(a)
        assert(tcpConnection.receiveMessage().get == requestToResponse(a))
      }
      tcpConnection.close()
      tcpToRequestResponse.close()
    }
  }

  "On error while executing request the TCP connection is closed" in {
    for (errorTrigger ← List(Error, ExecutionError)) {
      val (tcpToRequestResponse, tcpConnection) = newTcpToRequestResponse()
      tcpConnection.sendMessage(errorTrigger)
      tcpConnection.receiveMessage() shouldEqual None
      tcpConnection.close()
      tcpToRequestResponse.close()
    }
  }

  "TcpToRequestResponse.close closes TCP connection" in {
    val (tcpToRequestResponse, tcpConnection) = newTcpToRequestResponse()
    tcpToRequestResponse.close()
    tcpConnection.receiveMessage() shouldEqual None
    tcpConnection.close()
  }

  private def newTcpToRequestResponse(connectionMessage: Option[ByteString] = None) =
    autoClosing(BlockingTcpConnection.Listener.forLocalHostPort()) { listener ⇒
      val tcpToRequestResponse = new TcpToRequestResponse(actorSystem, listener.boundAddress, executeRequest, name = "TEST")
      tcpToRequestResponse.start(connectionMessage)
      val tcpConnection = listener.accept()
      (tcpToRequestResponse, tcpConnection)
    }
}

private object TcpToRequestResponseTest {
  private val Error = ByteString("ERROR")
  private val ExecutionError = ByteString("EXECUTION ERROR")

  private def executeRequest(request: ByteString) =
    request match {
      case ExecutionError ⇒ throw new RuntimeException("TEST EXECUTION")
      case Error ⇒ Future.failed(new RuntimeException("TEST"))
      case _ ⇒ Future.successful(requestToResponse(request))
    }

  private def requestToResponse(o: ByteString): ByteString = o ++ ByteString(" RESPONSE")
}
