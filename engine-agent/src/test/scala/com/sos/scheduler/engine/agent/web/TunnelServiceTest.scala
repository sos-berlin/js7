package com.sos.scheduler.engine.agent.web

import akka.actor.ActorSystem
import akka.util.ByteString
import com.sos.scheduler.engine.agent.web.TunnelServiceTest._
import com.sos.scheduler.engine.common.sprayutils.ByteStreamMarshallers._
import com.sos.scheduler.engine.tunnel.data.Http._
import com.sos.scheduler.engine.tunnel.data.TunnelId
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.concurrent.Future
import scala.util.Random
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.`application/octet-stream`
import spray.http.StatusCodes.OK
import spray.http.Uri
import spray.http.Uri.Path
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TunnelServiceTest extends FreeSpec with ScalatestRouteTest with TunnelService {

  implicit lazy val actorRefFactory = ActorSystem()
  //import actorRefFactory.dispatcher

  protected def tunnelRequest(tunnelIdWithPassword: TunnelId.WithPassword, requestMessage: ByteString) = {
    assert(tunnelIdWithPassword == TunnelId.WithPassword(TestTunnelId, TestPassword))
    Future.successful(requestToResponse(requestMessage))
  }

  TunnelPath.toString in {
    val requestMessage = ByteString.fromString(Random.nextString(10))
      Post(Uri.Empty withPath (TunnelPath / TestTunnelId.string), requestMessage) ~>
        addHeader(PasswordHeaderName, TestPassword.string) ~>
        Accept(`application/octet-stream`) ~>
        route ~> check
      {
        assert(status == OK)
        assert(responseAs[ByteString] == requestToResponse(requestMessage))
      }
  }

  private def requestToResponse(request: ByteString) = request ++ ByteString.fromString(" RESPONSE)")
}

private object TunnelServiceTest {
  private val TunnelPath = Path("/jobscheduler/agent/tunnel")
  private val CrazyString = """,.-;:_!"§$%&/()=#'+*´`<>"""
  private val TestTunnelId = TunnelId(CrazyString * 2)  // In practice (2015), the TunnelId is simpler
  private val TestPassword = TunnelId.Password(CrazyString.reverse * 100)  // In practice (2015), the password is simpler. See TunnelId.newPassword
}
