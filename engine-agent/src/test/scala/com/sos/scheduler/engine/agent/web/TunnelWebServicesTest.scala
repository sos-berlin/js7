package com.sos.scheduler.engine.agent.web

import akka.actor.ActorSystem
import akka.util.ByteString
import com.sos.scheduler.engine.agent.web.TunnelWebServicesTest._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.sprayutils.ByteStringMarshallers._
import com.sos.scheduler.engine.common.time.ScalaTime.DurationRichInt
import com.sos.scheduler.engine.common.time.alarm.AlarmClock
import com.sos.scheduler.engine.http.server.heartbeat.HeartbeatService
import com.sos.scheduler.engine.tunnel.data.Http._
import com.sos.scheduler.engine.tunnel.data._
import java.net.InetAddress
import java.time.{Duration, Instant}
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
final class TunnelWebServicesTest extends FreeSpec with ScalatestRouteTest with TunnelWebService {

  protected implicit lazy val actorRefFactory = ActorSystem()
  protected val heartbeatService = new HeartbeatService(new AlarmClock(100.ms))

  protected def tunnelRequest(tunnelToken: TunnelToken, requestMessage: ByteString, timeout: Option[Duration]) = {
    assert(tunnelToken == TunnelToken(TestTunnelId, TestSecret))
    require(timeout.isEmpty)
    Future.successful(requestToResponse(requestMessage))
  }

  protected def onTunnelHeartbeat(tunnelToken: TunnelToken): Unit = logger.debug(s"$tunnelToken: Heartbeat")
  protected def tunnelHandlerOverview = Future.successful(TestTunnelHandlerOverview)
  protected def tunnelOverviews = Future.successful(TestTunnelOverviews)

  s"GET $TunnelPath" in {
    Get(s"$TunnelPath") ~>
      Accept(MediaTypes.`application/json`) ~> route ~> check
    {
      assert(status == OK)
      assert(responseAs[TunnelHandlerOverview] == TestTunnelHandlerOverview)
    }
  }

  s"GET $TunnelPath/" in {
    Get(s"$TunnelPath/") ~>
      Accept(MediaTypes.`application/json`) ~> route ~> check
    {
      assert(status == OK)
      assert(responseAs[immutable.Seq[TunnelOverview]] == TestTunnelOverviews)
    }
  }

  s"POST $TunnelPath/ID" in {
    val requestMessage = ByteString(Random.nextString(10))
    Post(Uri.Empty withPath (TunnelPath / TestTunnelId.string), requestMessage) ~>
      addHeader(SecretHeaderName, TestSecret.string) ~>
      Accept(`application/octet-stream`) ~>
      route ~> check
    {
      assert(status == OK)
      assert(responseAs[ByteString] == requestToResponse(requestMessage))
    }
  }

  private def requestToResponse(request: ByteString) = request ++ ByteString(" RESPONSE)")
}

private object TunnelWebServicesTest {
  private val logger = Logger(getClass)
  private val TunnelPath = Path("/jobscheduler/agent/api/tunnel")
  private val CrazyString = """,.-;:_!"§$%&/()=#'+*´`<>"""
  private val TestTunnelId = TunnelId(CrazyString * 2)  // In practice (2015), the TunnelId is simpler
  private val TestSecret = TunnelToken.Secret(CrazyString.reverse * 100)  // In practice (2015), the secret is simpler. See TunnelId.newSecret
  private val TestTunnelHandlerOverview = TunnelHandlerOverview(Some("TCP-ADDRESS"), tunnelCount = 77)
  private val TestTunnelOverviews = List(
    TunnelOverview(
      TunnelId("TUNNEL-1"),
      startedByHttpIp = Some(InetAddress.getByName("127.1.2.3")),
      remoteTcpAddress = Some("REMOTE-ADDRESS"),
      TunnelStatistics(10, 1000, Some(Instant.parse("2015-07-03T12:00:00Z")), failure = Some("FAILURE"))))
}
