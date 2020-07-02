package js7.common.tcp

import akka.util.ByteString
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.tcp.TcpToRequestResponseTest._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import scala.concurrent.Future
import scala.util.control.NoStackTrace

/**
 * @author Joacim Zschimmer
 */
final class TcpToRequestResponseTest extends AnyFreeSpec with BeforeAndAfterAll
{
  private lazy val actorSystem = newActorSystem(getClass.getSimpleName)

  override protected def afterAll() = {
    Akkas.terminateAndWait(actorSystem, 99.s)
    super.afterAll()
  }

  for ((testName, connectionMessageOption) <- List("without connection message" -> None, "with connection message" -> Some(ByteString("Connection message")))) {
    s"Some requests, $testName" in {
      val (tcpToRequestResponse, tcpConnection) = newTcpToRequestResponse(connectionMessageOption)
      for (m <- connectionMessageOption) {
        assert(tcpConnection.receiveMessage().get == m)
      }
      for (i <- 1 to 3) {
        val a = ByteString(s"TEST #$i")
        tcpConnection.sendMessage(a)
        assert(tcpConnection.receiveMessage().get == requestToResponse(a))
      }
      tcpConnection.close()
      tcpToRequestResponse.close()
    }
  }

  "On error while executing request the TCP connection is closed" in {
    for (errorTrigger <- List(Error, ExecutionError)) {
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
    autoClosing(BlockingTcpConnection.Listener.forLocalHostPort()) { listener =>
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
      case ExecutionError => throw new RuntimeException("TEST EXECUTION") with NoStackTrace
      case Error => Future.failed(new RuntimeException("TEST") with NoStackTrace)
      case _ => Future.successful(requestToResponse(request))
    }

  private def requestToResponse(o: ByteString): ByteString = o ++ ByteString(" RESPONSE")
}
