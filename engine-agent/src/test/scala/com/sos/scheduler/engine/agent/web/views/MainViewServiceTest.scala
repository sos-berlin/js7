package com.sos.scheduler.engine.agent.web.views

import akka.actor.ActorSystem
import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.views.ProcessOverview
import com.sos.scheduler.engine.agent.process.ProcessHandlerView
import com.sos.scheduler.engine.agent.views.AgentOverview
import com.sos.scheduler.engine.common.sprayutils.JsObjectMarshallers._
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.{`application/json`, `text/plain`}
import spray.http.Uri
import spray.json._
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class MainViewServiceTest extends FreeSpec with ScalatestRouteTest with MainViewService {

  implicit lazy val actorRefFactory = ActorSystem()

  protected def processHandlerView = new ProcessHandlerView {
    def currentProcessCount = 777
    def totalProcessCount = 999
    def processes = List(ProcessOverview(AgentProcessId("1-123"), controllerAddress = "127.0.0.1:999999999", Instant.parse("2015-06-10T12:00:00Z")))
    def isTerminating = false
  }

  protected def agentOverview = AgentOverview(
    startedAt = Instant.parse("2015-06-01T12:00:00Z"),
    version = "TEST-VERSION",
    currentProcessCount = processHandlerView.currentProcessCount,
    totalProcessCount = processHandlerView.totalProcessCount,
    isTerminating = false,
    system = AgentOverview.SystemInformation(hostname = "TEST-HOSTNAME"),
    java = AgentOverview.JavaInformation(systemProperties = Map("test" → "TEST")))

  private def expectedOverviewJsObject = JsObject(
    "startedAt" → JsString("2015-06-01T12:00:00Z"),
    "version" → JsString("TEST-VERSION"),
    "currentProcessCount" → JsNumber(777),
    "totalProcessCount" → JsNumber(999),
    "isTerminating" → JsBoolean(false),
    "system" → JsObject(
      "hostname" → JsString("TEST-HOSTNAME")),
    "java" → JsObject(
      "systemProperties" → JsObject(
        "test" → JsString("TEST"))))

  "overview" - {
    "Accept: application/json returns compact JSON" in {
      Get(Uri("/jobscheduler/agent/overview")) ~> Accept(`application/json`) ~> route ~> check {
        assert(responseAs[JsObject] == expectedOverviewJsObject)
        assert(!(responseAs[String] contains " ")) // Compact JSON
      }
    }

    "Accept: text/plain returns pretty YAML" in {
      // curl http://.../jobscheduler/agent/overview shows user readable json
      Get(Uri("/jobscheduler/agent/overview")) ~> Accept(`text/plain`) ~> route ~> check {
        assert(responseAs[String] contains " ") // YAML
      }
    }
  }
}
