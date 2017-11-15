package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.headers.Accept
import com.sos.jobscheduler.agent.command.{CommandHandler, CommandHandlerDetailed, CommandHandlerOverview, CommandMeta, CommandRunOverview, InternalCommandId}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand._
import com.sos.jobscheduler.agent.web.CommandWebServiceTest._
import com.sos.jobscheduler.agent.web.test.WebServiceTest
import com.sos.jobscheduler.common.akkahttp.JsObjectMarshallers._
import com.sos.jobscheduler.common.akkahttp.JsonString
import com.sos.jobscheduler.common.time.ScalaTime._
import java.time.Instant
import org.scalatest.FreeSpec
import scala.concurrent.Future
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class CommandWebServiceTest extends FreeSpec with WebServiceTest with CommandWebService {

  protected implicit def executionContext = system.dispatcher
  override protected val uriPathPrefix = "test"

  protected val commandHandler = new CommandHandler {
    def execute(command: AgentCommand, meta: CommandMeta) =
      Future.successful {
        command match {
          case TestCommand ⇒ AgentCommand.Accepted
          case _ ⇒ fail()
        }
      }

    def overview = Future.successful(CommandHandlerOverview(currentCommandCount = 111, totalCommandCount = 222))

    def detailed = Future.successful(CommandHandlerDetailed(List(
      CommandRunOverview(InternalCommandId(333), Instant.parse("2015-06-22T12:00:00Z"), TestCommand))))
  }

  "Terminate" in {
    val json = """{
        "TYPE": "Terminate",
        "sigtermProcesses": false,
        "sigkillProcessesAfter": "PT999S"
      }"""
    postJsonCommand(json) ~> check {
      assert(responseAs[AgentCommand.Accepted.type] == AgentCommand.Accepted)
      assert(responseAs[String].parseJson == """{ "TYPE": "Accepted" }""".parseJson)
    }
  }

  private def postJsonCommand(json: String): RouteTestResult =
    Post("/test/agent/api/command", JsonString(json)) ~>
      Accept(`application/json`) ~>
      route

  "commandHandler returns overview" in {
    Get("/test/agent/api/command") ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[JsObject] == JsObject(
        "currentCommandCount" → JsNumber(111),
        "totalCommandCount" → JsNumber(222)))
    }
  }

  "commandHandler/ returns array of running command" in {
    Get("/test/agent/api/command/") ~> Accept(`application/json`) ~> route ~> check {
      assert(status == OK)
      assert(responseAs[JsArray] == JsArray(
        JsObject(
          "internalId" → JsString("333"),
          "startedAt" → JsString("2015-06-22T12:00:00Z"),
          "command" → (TestCommand: AgentCommand).toJson)))
    }
  }
}

private object CommandWebServiceTest {
  private val TestCommand = Terminate(sigkillProcessesAfter = Some(999.s))
}
