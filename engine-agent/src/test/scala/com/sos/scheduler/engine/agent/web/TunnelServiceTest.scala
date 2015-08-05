package com.sos.scheduler.engine.agent.web

import akka.actor.ActorSystem
import akka.util.ByteString
import com.sos.scheduler.engine.agent.web.TunnelServiceTest._
import com.sos.scheduler.engine.common.sprayutils.ByteStreamMarshallers._
import com.sos.scheduler.engine.tunnel.data.Http._
import com.sos.scheduler.engine.tunnel.data._
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.collection.immutable
import scala.concurrent.Future
import scala.util.Random
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.`application/octet-stream`
import spray.http.StatusCodes.OK
import spray.http.Uri.Path
import spray.http.{MediaTypes, Uri}
import spray.httpx.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class TunnelServiceTest extends FreeSpec with ScalatestRouteTest with TunnelService {

  protected implicit lazy val actorRefFactory = ActorSystem()

  protected def tunnelRequest(tunnelToken: TunnelToken, requestMessage: ByteString) = {
    assert(tunnelToken == TunnelToken(TestTunnelId, TestSecret))
    Future.successful(requestToResponse(requestMessage))
  }

  protected def tunnelHandlerOverview = Future.successful(TestTunnelHandlerOverview)
  protected def tunnelOverviews = Future.successful(TestTunnelOverviews)

  s"POST $TunnelPath/item" in {
    val requestMessage = ByteString.fromString(Random.nextString(10))
    Post(Uri.Empty withPath (TunnelPath / "item" / TestTunnelId.string), requestMessage) ~>
      addHeader(SecretHeaderName, TestSecret.string) ~>
      Accept(`application/octet-stream`) ~>
      route ~> check
    {
      assert(status == OK)
      assert(responseAs[ByteString] == requestToResponse(requestMessage))
    }
  }

  s"GET $TunnelPath" in {
    Get(Uri.Empty withPath TunnelPath) ~>
      Accept(MediaTypes.`application/json`) ~> route ~> check
    {
      assert(status == OK)
      assert(responseAs[TunnelHandlerOverview] == TestTunnelHandlerOverview)
    }
  }

  s"GET $TunnelPath/details" in {
    Get(Uri.Empty withPath TunnelPath / "details") ~>
      Accept(MediaTypes.`application/json`) ~> route ~> check
    {
      assert(status == OK)
      assert(responseAs[immutable.Seq[TunnelOverview]] == TestTunnelOverviews)
    }
  }

  private def requestToResponse(request: ByteString) = request ++ ByteString.fromString(" RESPONSE)")
}

private object TunnelServiceTest {
  private val TunnelPath = Path("/jobscheduler/agent/tunnels")
  private val CrazyString = """,.-;:_!"§$%&/()=#'+*´`<>"""
  private val TestTunnelId = TunnelId(CrazyString * 2)  // In practice (2015), the TunnelId is simpler
  private val TestSecret = TunnelToken.Secret(CrazyString.reverse * 100)  // In practice (2015), the secret is simpler. See TunnelId.newSecret
  private val TestTunnelHandlerOverview = TunnelHandlerOverview(Some("TCP-ADDRESS"), tunnelCount = 77)
  private val TestTunnelOverviews = List(
    TunnelOverview(
      TunnelId("TUNNEL-1"),
      remoteTcpAddress = Some("REMOTE-ADDRESS"),
      TunnelStatistics(10, 1000, Some(Instant.parse("2015-07-03T12:00:00Z")), failure = Some("FAILURE"))))
}
