package com.sos.scheduler.engine.agent.web

import akka.actor.ActorSystem
import com.sos.scheduler.engine.agent.data.commandresponses.{EmptyResponse, FileOrderSourceContent}
import com.sos.scheduler.engine.agent.data.commands.{Command, Terminate, _}
import com.sos.scheduler.engine.agent.web.CommandServiceTest._
import com.sos.scheduler.engine.base.exceptions.StandardPublicException
import com.sos.scheduler.engine.common.soslicense.LicenseKeyChecker
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.time.Duration
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.concurrent.Future
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.`application/json`
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

  protected implicit lazy val actorRefFactory = ActorSystem()
  override protected val uriPathPrefix = "test"

  protected def executeCommand(command: Command, licenseKey: Option[LicenseKeyChecker]) =
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
      assert(status == InternalServerError)
      assert(responseAs[String] startsWith "TEST EXCEPTION")
    }
  }

  private def postJsonCommand(json: String): RouteResult =
    Post("/test/jobscheduler/agent/api/command", json)(stringMarshaller(`application/json`)) ~>
      Accept(`application/json`) ~>
      route
}

object CommandServiceTest {
  private val KnownFile = "/DIRECTORY/known"
  private val TestRequestFileOrderSourceContent = RequestFileOrderSourceContent("/DIRECTORY", ".*", Duration.ofMillis(111222333444555666L), Set(KnownFile))
  private val FailingRequestFileOrderSourceContent = RequestFileOrderSourceContent("ERROR", "", Duration.ZERO, Set())
  private val TestFileOrderSourceContent = FileOrderSourceContent(List(
    FileOrderSourceContent.Entry("/DIRECTORY/a", 111222333444555666L)))
}
