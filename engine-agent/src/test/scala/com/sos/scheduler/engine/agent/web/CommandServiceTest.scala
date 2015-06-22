package com.sos.scheduler.engine.agent.web

import akka.actor.ActorSystem
import com.sos.scheduler.engine.agent.data.commands.{Command, Terminate, _}
import com.sos.scheduler.engine.agent.data.responses.{EmptyResponse, FileOrderSourceContent}
import com.sos.scheduler.engine.agent.web.CommandServiceTest._
import com.sos.scheduler.engine.base.exceptions.StandardPublicException
import com.sos.scheduler.engine.common.time.ScalaTime._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.concurrent.Future
import scala.xml.XML
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.{`application/json`, `application/xml`}
import spray.http.StatusCodes.InternalServerError
import spray.httpx.SprayJsonSupport._
import spray.httpx.marshalling.BasicMarshallers.stringMarshaller
import spray.json._
import spray.testkit.ScalatestRouteTest

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class CommandServiceTest extends FreeSpec with ScalatestRouteTest with CommandService {

  implicit lazy val actorRefFactory = ActorSystem()

  protected def executeCommand(command: Command) =
    Future.successful {
      val expectedTerminate = Terminate(sigkillProcessesAfter = Some(999.s))
      command match {
        case TestRequestFileOrderSourceContent ⇒ TestFileOrderSourceContent
        case FailingRequestFileOrderSourceContent ⇒ throw new StandardPublicException(s"TEST EXCEPTION: $command")
        case `expectedTerminate` ⇒ EmptyResponse
      }
    }

  "jobscheduler/agent/command for JSON" - {
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

    "Terminate" in {
      val json = """{
          "$TYPE": "Terminate",
          "sigtermProcesses": false,
          "sigkillProcessesAfter": "PT999S"
        }"""
      postJsonCommand(json) ~> check {
        assert(responseAs[EmptyResponse.type] == EmptyResponse)
        assert(responseAs[String].parseJson == "{}".parseJson)
      }
    }

    "Exception (JSON)" in {
      val json = """{
          "$TYPE": "RequestFileOrderSourceContent",
          "directory": "ERROR",
          "regex": "",
          "durationMillis": 0,
          "knownFiles": []
        }"""
      postJsonCommand(json) ~> check {
        assert(status == InternalServerError)
        assert(responseAs[String] startsWith "TEST EXCEPTION")
      }
    }

    def postJsonCommand(json: String): RouteResult =
      Post("/jobscheduler/agent/command", json)(stringMarshaller(`application/json`)) ~>
        Accept(`application/json`) ~>
        route
  }

  "jobscheduler/agent/command for XML commands" - {
    "agent.terminate" in {
      postXmlCommand(<agent.terminate cmd="terminate" timeout="999"/>) ~> check {
        assert(XML.loadString(responseAs[String]) == <spooler><answer><ok/></answer></spooler> )
      }
    }

    "Unknown XML command" in {
      postXmlCommand(<ERROR/>) ~> check {
        assert(status == InternalServerError)
        assert(responseAs[String] startsWith "Unexpected XML element <ERROR>")
      }
    }

    def postXmlCommand(elem: xml.Elem): RouteResult =
      Post("/jobscheduler/agent/command", elem) ~> Accept(`application/xml`) ~> route
  }
}

object CommandServiceTest {
  private val KnownFile = "/DIRECTORY/known"
  private val TestRequestFileOrderSourceContent = RequestFileOrderSourceContent("/DIRECTORY", ".*", 111222333444555666L, Set(KnownFile))
  private val FailingRequestFileOrderSourceContent = RequestFileOrderSourceContent("ERROR", "", 0, Set())
  private val TestFileOrderSourceContent = FileOrderSourceContent(List(
    FileOrderSourceContent.Entry("/DIRECTORY/a", 111222333444555666L)))
}
