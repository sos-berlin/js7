package com.sos.scheduler.engine.agent.web

import akka.actor.ActorSystem
import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.views.ProcessOverview
import com.sos.scheduler.engine.agent.process.ProcessHandlerView
import com.sos.scheduler.engine.agent.views.AgentOverview
import com.sos.scheduler.engine.agent.web.marshal.JsObjectMarshallers._
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.scheduler.engine.common.scalautil.Closers.withCloser
import com.sos.scheduler.engine.common.scalautil.Logger
import java.nio.file.Files
import java.nio.file.Files.createTempFile
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.{`application/json`, `text/plain`}
import spray.http.StatusCodes.{NotFound, OK}
import spray.http.Uri
import spray.json._
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ViewServiceTest extends FreeSpec with ScalatestRouteTest with ViewService {

  implicit lazy val actorRefFactory = ActorSystem()

    protected def processHandlerView = new ProcessHandlerView {
      def currentProcessCount = 777

      def totalProcessCount = 999

      def processes = List(ProcessOverview(AgentProcessId("1-123"), controllerAddress = "127.0.0.1:999999999", Instant.parse("2015-06-10T12:00:00Z")))
    }

  protected def agentOverview = AgentOverview(
    startedAt = Instant.parse("2015-06-01T12:00:00Z"),
    version = "TEST-VERSION",
    currentProcessCount = processHandlerView.currentProcessCount,
    totalProcessCount = processHandlerView.totalProcessCount)

  private def expectedOverviewJsObject = JsObject(
    "startedAt" → JsString("2015-06-01T12:00:00Z"),
    "version" → JsString("TEST-VERSION"),
    "currentProcessCount" → JsNumber(777),
    "totalProcessCount" → JsNumber(999))

  "fileStatus" in {
    withCloser { implicit closer ⇒
      val file = createTempFile("test-", ".tmp") withCloser Files.delete
      Get(Uri("/jobscheduler/agent/fileStatus").withQuery("file" → file.toString)) ~> Accept(`application/json`) ~> viewRoute ~> check {
        assert(status == OK)
      }
      Get(Uri("/jobscheduler/agent/fileStatus").withQuery("file" → "--UNKNOWN--")) ~> Accept(`application/json`) ~> viewRoute ~> check {
        assert(status == NotFound)
      }
    }
  }

  "overview" - {
    "Accept: application/json returns compact JSON" in {
      Get(Uri("/jobscheduler/agent/overview")) ~> Accept(`application/json`) ~> viewRoute ~> check {
        assert(responseAs[JsObject] == expectedOverviewJsObject)
        assert(!(responseAs[String] contains " ")) // Compact JSON
      }
    }

    "Accept: text/plain returns pretty JSON" in {
      // curl http://.../jobscheduler/agent/overview shows user readable json
      Get(Uri("/jobscheduler/agent/overview")) ~> Accept(`text/plain`) ~> viewRoute ~> check {
        val string = responseAs[String]
        assert(JsonParser(string) == expectedOverviewJsObject)
        assert(responseAs[String] contains " ") // PrettyJSON
      }
    }
  }

  "processHandler" in {
    Get(Uri("/jobscheduler/agent/processHandler")) ~> Accept(`application/json`) ~> viewRoute ~> check {
      assert(responseAs[JsObject] == JsObject(
        "processes" → JsArray(
          JsObject(
            "id" → JsString("1-123"),
            "controllerAddress" → JsString("127.0.0.1:999999999"),
            "startedAt" → JsString("2015-06-10T12:00:00Z"))
        )))
    }
  }

}

object ViewServiceTest {
  private val logger = Logger(getClass)
}
