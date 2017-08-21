package com.sos.jobscheduler.agent.web

import com.sos.jobscheduler.agent.command.CommandMeta
import com.sos.jobscheduler.agent.data.commandresponses.EmptyResponse
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand._
import com.sos.jobscheduler.agent.web.test.WebServiceTest
import com.sos.jobscheduler.common.time.ScalaTime._
import org.scalatest.FreeSpec
import scala.concurrent.Future
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes.`application/json`
import spray.httpx.SprayJsonSupport._
import spray.httpx.marshalling.BasicMarshallers.stringMarshaller
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class CommandWebServiceTest extends FreeSpec with WebServiceTest with CommandWebService {

  protected implicit def executionContext = system.dispatcher
  override protected val uriPathPrefix = "test"

  protected def executeCommand(command: AgentCommand, meta: CommandMeta) =
    Future.successful {
      val expectedTerminate = Terminate(sigkillProcessesAfter = Some(999.s))
      command match {
        case `expectedTerminate` ⇒ EmptyResponse
        case _ ⇒ fail()
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

  private def postJsonCommand(json: String): RouteResult =
    Post("/test/jobscheduler/agent/api/command", json)(stringMarshaller(`application/json`)) ~>
      Accept(`application/json`) ~>
      route
}
