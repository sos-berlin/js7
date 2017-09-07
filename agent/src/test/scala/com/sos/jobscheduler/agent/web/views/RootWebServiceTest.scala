package com.sos.jobscheduler.agent.web.views

import akka.http.scaladsl.model.MediaTypes.{`application/json`, `text/plain`}
import akka.http.scaladsl.model.headers.Accept
import com.sos.jobscheduler.agent.views.AgentOverview
import com.sos.jobscheduler.agent.web.test.WebServiceTest
import com.sos.jobscheduler.base.system.SystemInformation
import com.sos.jobscheduler.common.akkahttp.JsObjectMarshallers._
import com.sos.jobscheduler.data.system.JavaInformation
import java.time.Instant
import org.scalatest.FreeSpec
import scala.concurrent.Future
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class RootWebServiceTest extends FreeSpec with WebServiceTest with RootWebService {

  protected def executionContext = actorSystem.dispatcher
  protected def agentOverview = Future.successful(AgentOverview(
    startedAt = Instant.parse("2015-06-01T12:00:00Z"),
    version = "TEST-VERSION",
    isTerminating = false,
    system = SystemInformation(hostname = "TEST-HOSTNAME"),
    java = JavaInformation(
      systemProperties = Map("test" → "TEST"),
      JavaInformation.Memory(maximum = 3, total = 2, free = 1))))

  private def expectedOverviewJsObject = """{
    "startedAt": "2015-06-01T12:00:00Z",
    "version": "TEST-VERSION",
    "isTerminating": false,
    "system": {
      "hostname": "TEST-HOSTNAME",
      "mxBeans": {}
    },
    "java": {
      "systemProperties": {
        "test": "TEST"
      },
      "memory": {
        "maximum": 3,
        "total": 2,
        "free": 1
      }
    }
  }""".parseJson

  "overview" - {
    "Accept: application/json returns compact JSON" in {
      Get("/jobscheduler/agent/api") ~> Accept(`application/json`) ~> route ~> check {
        assert(responseAs[JsObject] == expectedOverviewJsObject)
        assert(!(responseAs[String] contains " ")) // Compact JSON
      }
    }

    "Accept: text/plain returns pretty YAML" in {
      Get("/jobscheduler/agent/api") ~> Accept(`text/plain`) ~> route ~> check {
        assert(responseAs[String] contains " ") // YAML
      }
    }
  }
}
