package com.sos.jobscheduler.agent.web.views

import akka.http.scaladsl.model.MediaTypes.{`application/json`, `text/plain`}
import akka.http.scaladsl.model.headers.Accept
import com.sos.jobscheduler.agent.data.views.AgentOverview
import com.sos.jobscheduler.agent.web.test.WebServiceTest
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.system.SystemInformation
import com.sos.jobscheduler.common.http.AkkaHttpUtils.RichHttpResponse
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.system.JavaInformation
import io.circe.Json
import java.time.Instant
import org.scalatest.FreeSpec
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class RootWebServiceTest extends FreeSpec with WebServiceTest with RootWebService {

  protected def executionContext = actorSystem.dispatcher
  protected def agentOverview = Future.successful(AgentOverview(
    startedAt = Instant.parse("2015-06-01T12:00:00Z"),
    version = "TEST-VERSION",
    buildId = "BUILD-ID",
    isTerminating = false,
    system = SystemInformation(hostname = "TEST-HOSTNAME"),
    java = JavaInformation(
      version = "x.y.z",
      JavaInformation.Memory(maximum = 3, total = 2, free = 1),
      systemProperties = Map("test" → "TEST"))))

  private def expectedOverviewJson = json"""{
    "startedAt": "2015-06-01T12:00:00Z",
    "version": "TEST-VERSION",
    "buildId": "BUILD-ID",
    "isTerminating": false,
    "system": {
      "hostname": "TEST-HOSTNAME",
      "mxBeans": {}
    },
    "java": {
      "version": "x.y.z",
      "memory": {
        "maximum": 3,
        "total": 2,
        "free": 1
      },
      "systemProperties": {
        "test": "TEST"
      }
    }
  }"""

  "overview" - {
    "Accept: application/json returns compact JSON" in {
      Get("/agent/api") ~> Accept(`application/json`) ~> route ~> check {
        assert(responseAs[Json] == expectedOverviewJson)
        assert(!(response.utf8StringFuture.await(99.s) contains " ")) // Compact JSON
      }
    }

    "Accept: text/plain returns pretty YAML" in {
      Get("/agent/api") ~> Accept(`text/plain`) ~> route ~> check {
        assert(response.utf8StringFuture.await(99.s) contains " ") // YAML
      }
    }
  }
}
