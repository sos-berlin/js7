package com.sos.scheduler.engine.agent.web.views

import com.sos.scheduler.engine.agent.views.AgentOverview
import com.sos.scheduler.engine.agent.web.test.WebServiceTest
import com.sos.scheduler.engine.common.sprayutils.JsObjectMarshallers._
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.{`application/json`, `text/plain`}
import spray.json._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class RootWebServiceTest extends FreeSpec with WebServiceTest with RootWebService {

  protected def agentOverview = AgentOverview(
    startedAt = Instant.parse("2015-06-01T12:00:00Z"),
    version = "TEST-VERSION",
    currentTaskCount = 777,
    totalTaskCount = 999,
    isTerminating = false,
    system = AgentOverview.SystemInformation(hostname = "TEST-HOSTNAME"),
    java = AgentOverview.JavaInformation(systemProperties = Map("test" → "TEST")))

  private def expectedOverviewJsObject = JsObject(
    "startedAt" → JsString("2015-06-01T12:00:00Z"),
    "version" → JsString("TEST-VERSION"),
    "currentTaskCount" → JsNumber(777),
    "totalTaskCount" → JsNumber(999),
    "isTerminating" → JsBoolean(false),
    "system" → JsObject(
      "hostname" → JsString("TEST-HOSTNAME"),
      "mxBeans" → JsObject()),
    "java" → JsObject(
      "systemProperties" → JsObject(
        "test" → JsString("TEST"))))

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
