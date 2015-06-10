package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.configuration.Akkas._
import com.sos.scheduler.engine.agent.data.AgentProcessId
import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.data.responses.{FileOrderSourceContent, Response, StartProcessResponse}
import com.sos.scheduler.engine.agent.data.views.ProcessOverview
import com.sos.scheduler.engine.agent.process.ProcessHandlerView
import com.sos.scheduler.engine.agent.views.AgentOverview
import com.sos.scheduler.engine.agent.web.AgentWebServiceTest._
import com.sos.scheduler.engine.agent.web.marshal.JsObjectMarshallers._
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAny
import com.sos.scheduler.engine.common.scalautil.HasCloser
import java.nio.file.Files
import java.nio.file.Files.createTempFile
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.Future
import scala.xml.XML
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.{`application/json`, `text/plain`}
import spray.http.StatusCodes.{InternalServerError, NotFound, OK}
import spray.http.Uri
import spray.httpx.SprayJsonSupport._
import spray.httpx.marshalling.BasicMarshallers.stringMarshaller
import spray.json._
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class AgentWebServiceTest extends FreeSpec with BeforeAndAfterAll with ScalatestRouteTest with AgentWebService with HasCloser {

  implicit lazy val actorRefFactory = newActorSystem("TEST")(closer)

  override def afterAll(): Unit = {
    close()
    super.afterAll()
  }

  "jobscheduler/engine/command for legacy XML commands" - {
    "remote_scheduler.start_remote_task" in {
      postXmlCommand(<remote_scheduler.start_remote_task tcp_port='999'/>) ~> check {
        assert(XML.loadString(responseAs[String]) == <spooler><answer><process process_id="123"/></answer></spooler>)
      }
    }

    "Unknown XML command" in {
      postXmlCommand(<ERROR/>) ~> check {
        assert(status == InternalServerError)
      }
    }
  }

  private def postXmlCommand(command: xml.Elem): RouteResult =
    Post("/jobscheduler/engine/command", command) ~>
      addHeader("Remote-Address", "0.0.0.0") ~>   // For this IP-less test only. Client's IP is normally set by configuration spray.can.remote-address-header
      route

  "jobscheduler/agent/command" - {
    "RequestFileOrderSourceContent" in {
      val json = """{
          "$TYPE": "RequestFileOrderSourceContent",
          "directory": "/DIRECTORY",
          "regex": ".*",
          "durationMillis": 111222333444555666,
          "knownFiles": [ "/DIRECTORY/known" ]
        }"""
      postJsonCommand(json) ~> check {
        assert(responseAs[FileOrderSourceContent] == TestFileOrderSourceContent)
        assert(responseAs[String].parseJson ==
          """{
          "files": [
            { "path": "/DIRECTORY/a", "lastModifiedTime": 111222333444555666 }
          ]
        }""".parseJson)
      }
    }

    "Exception" in {
      val json = """{
          "$TYPE": "RequestFileOrderSourceContent",
          "directory": "ERROR",
          "regex": "",
          "durationMillis": 0,
          "knownFiles": []
        }"""
      postJsonCommand(json) ~> check {
        assert(status == InternalServerError)
      }
    }
  }

  private def postJsonCommand(json: String): RouteResult = {
    Post("/jobscheduler/agent/command", json)(stringMarshaller(`application/json`)) ~>
      Accept(`application/json`) ~>
      route
  }

  protected def executeCommand(command: Command): Future[Response] =
    Future.successful[Response] {
      command match {
        case StartSeparateProcess("0.0.0.0:999", "", "") ⇒ StartProcessResponse(AgentProcessId(123))
        case TestRequestFileOrderSourceContent ⇒ TestFileOrderSourceContent
        case FailingRequestFileOrderSourceContent ⇒ throw new Exception(s"TEST EXCEPTION: $command")
      }
    }

  "fileStatus" in {
    val file = createTempFile("test-", ".tmp") withCloser Files.delete
    Get(Uri("/jobscheduler/agent/fileStatus").withQuery("file" → file.toString)) ~> Accept(`application/json`) ~> route ~> check {
      assert(status == OK)
    }
    Get(Uri("/jobscheduler/agent/fileStatus").withQuery("file" → "--UNKNOWN--")) ~> Accept(`application/json`) ~> route ~> check {
      assert(status == NotFound)
    }
  }

  "overview" - {
    "Accept: application/json returns compact JSON" in {
      Get(Uri("/jobscheduler/agent/overview")) ~> Accept(`application/json`) ~> route ~> check {
        assert(responseAs[JsObject] == expectedOverviewJsObject)
        assert(!(responseAs[String] contains " ")) // Compact JSON
      }
    }

    "Accept: text/plain returns pretty JSON" in {
      // curl http://.../jobscheduler/agent/overview shows user readable json
      Get(Uri("/jobscheduler/agent/overview")) ~> Accept(`text/plain`) ~> route ~> check {
        val string = responseAs[String]
        assert(JsonParser(string) == expectedOverviewJsObject)
        assert(responseAs[String] contains " ") // PrettyJSON
      }
    }
  }

  "processHandler" in {
    Get(Uri("/jobscheduler/agent/processHandler")) ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[JsObject] == JsObject(
        "processes" → JsArray(
          JsObject(
            "id" → JsNumber(123),
            "controllerAddress" → JsString("127.0.0.1:999999999"),
            "startedAt" → JsString("2015-06-10T12:00:00Z"))
        )))
    }
  }

  protected def processHandlerView = new ProcessHandlerView {
    def currentProcessCount = 777
    def totalProcessCount = 999
    def processes = List(ProcessOverview(AgentProcessId(123), controllerAddress = "127.0.0.1:999999999", Instant.parse("2015-06-10T12:00:00Z")))
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
}

private object AgentWebServiceTest {
  private val KnownFile = "/DIRECTORY/known"
  private val TestRequestFileOrderSourceContent = RequestFileOrderSourceContent("/DIRECTORY", ".*", 111222333444555666L, Set(KnownFile))
  private val FailingRequestFileOrderSourceContent = RequestFileOrderSourceContent("ERROR", "", 0, Set())
  private val TestFileOrderSourceContent = FileOrderSourceContent(List(
    FileOrderSourceContent.Entry("/DIRECTORY/a", 111222333444555666L)))
}
