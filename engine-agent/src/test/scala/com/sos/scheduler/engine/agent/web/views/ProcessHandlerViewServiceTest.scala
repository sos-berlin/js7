package com.sos.scheduler.engine.agent.web.views

import akka.actor.ActorSystem
import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.views.ProcessOverview
import com.sos.scheduler.engine.agent.process.ProcessHandlerView
import com.sos.scheduler.engine.common.sprayutils.JsObjectMarshallers._
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.`application/json`
import spray.http.Uri
import spray.json._
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ProcessHandlerViewServiceTest extends FreeSpec with ScalatestRouteTest with ProcessHandlerViewService {

  implicit lazy val actorRefFactory = ActorSystem()

  protected def processHandlerView = new ProcessHandlerView {
    def currentProcessCount = 777
    def totalProcessCount = 999
    def processes = List(ProcessOverview(AgentProcessId("1-123"), controllerAddress = "127.0.0.1:999999999", Instant.parse("2015-06-10T12:00:00Z")))
    def isTerminating = false
  }

  "processHandler" in {
    Get(Uri("/jobscheduler/agent/processHandler")) ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[JsObject] == JsObject(
        "processes" → JsArray(
          JsObject(
            "id" → JsString("1-123"),
            "controllerAddress" → JsString("127.0.0.1:999999999"),
            "startedAt" → JsString("2015-06-10T12:00:00Z")))))
    }
  }
}
