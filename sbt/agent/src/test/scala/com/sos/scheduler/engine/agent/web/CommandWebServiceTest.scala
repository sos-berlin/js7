package com.sos.scheduler.engine.agent.web

import com.sos.scheduler.engine.agent.command.CommandMeta
import com.sos.scheduler.engine.agent.data.commandresponses.{EmptyResponse, FileOrderSourceContent}
import com.sos.scheduler.engine.agent.data.commands._
import com.sos.scheduler.engine.agent.web.CommandWebServiceTest._
import com.sos.scheduler.engine.agent.web.test.WebServiceTest
import com.sos.scheduler.engine.base.exceptions.StandardPublicException
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.time.Duration
import org.scalatest.FreeSpec
import scala.concurrent.Future
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.`application/json`
import spray.http.StatusCodes.BadRequest
import spray.httpx.SprayJsonSupport._
import spray.httpx.marshalling.BasicMarshallers.stringMarshaller
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class CommandWebServiceTest extends FreeSpec with WebServiceTest with CommandWebService {

  protected implicit def executionContext = actorRefFactory.dispatcher
  override protected val uriPathPrefix = "test"

  protected def executeCommand(command: Command, meta: CommandMeta) =
    Future.successful {
      val expectedTerminate = Terminate(sigkillProcessesAfter = Some(999.s))
      command match {
        case TestRequestFileOrderSourceContent ⇒ TestFileOrderSourceContent
        case FailingRequestFileOrderSourceContent ⇒ throw new StandardPublicException(s"TEST EXCEPTION: $command")
        case `expectedTerminate` ⇒ EmptyResponse
      }
    }

  "RequestFileOrderSourceContent" in {
    val json = """{
        "$TYPE": "RequestFileOrderSourceContent",
        "directory": "/DIRECTORY",
        "regex": ".*",
        "duration": 111222333444555.666,
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
        "duration": 0,
        "knownFiles": []
      }"""
    postJsonCommand(json) ~> check {
      assert(status == BadRequest)
      assert(responseAs[String] startsWith "TEST EXCEPTION")
    }
  }

  private def postJsonCommand(json: String): RouteResult =
    Post("/test/jobscheduler/agent/api/command", json)(stringMarshaller(`application/json`)) ~>
      Accept(`application/json`) ~>
      route
}

object CommandWebServiceTest {
  private val KnownFile = "/DIRECTORY/known"
  private val TestRequestFileOrderSourceContent = RequestFileOrderSourceContent("/DIRECTORY", ".*", Duration.ofMillis(111222333444555666L), Set(KnownFile))
  private val FailingRequestFileOrderSourceContent = RequestFileOrderSourceContent("ERROR", "", Duration.ZERO, Set())
  private val TestFileOrderSourceContent = FileOrderSourceContent(List(
    FileOrderSourceContent.Entry("/DIRECTORY/a", 111222333444555666L)))
}
