package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.headers.Accept
import com.sos.jobscheduler.agent.command.{CommandHandler, CommandHandlerDetailed, CommandHandlerOverview, CommandMeta, CommandRunOverview, InternalCommandId}
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand._
import com.sos.jobscheduler.agent.web.CommandWebServiceTest._
import com.sos.jobscheduler.agent.web.test.WebServiceTest
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import io.circe.Json
import io.circe.syntax.EncoderOps
import java.time.Instant
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.concurrent.Future
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
final class CommandWebServiceTest extends FreeSpec with WebServiceTest with CommandWebService {

  protected def scheduler = Scheduler.global
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

  private val route =
    pathSegments("agent/api/command") {
      commandRoute
    }

  "Terminate" in {
    val json = json"""{
        "TYPE": "Terminate",
        "sigtermProcesses": false,
        "sigkillProcessesAfter": 999
      }"""
    postJsonCommand(json) ~> check {
      if (status != OK) fail(s"$status - ${responseEntity.toStrict(9.seconds).value}")
      assert(responseAs[AgentCommand.Accepted.type] == AgentCommand.Accepted)
      assert(responseEntity.toStrict(9.seconds).value.get.get.data.utf8String.parseJson ==
        """{ "TYPE": "Accepted" }""".parseJson)
    }
  }

  private def postJsonCommand(json: Json): RouteTestResult =
    Post("/agent/api/command", json) ~>
      Accept(`application/json`) ~>
      route

  "commandHandler returns overview" in {
    Get("/agent/api/command") ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[Json] == Json.obj(
        "currentCommandCount" → Json.fromInt(111),
        "totalCommandCount" → Json.fromInt(222)))
    }
  }

  "commandHandler/ returns array of running command" in {
    Get("/agent/api/command/") ~> Accept(`application/json`) ~> route ~> check {
      assert(status == OK)
      assert(responseAs[Json] == Json.fromValues(List(
        Json.obj(
          "internalId" → Json.fromString("333"),
          "startedAt" → Json.fromString("2015-06-22T12:00:00Z"),
          "command" → (TestCommand: AgentCommand).asJson))))
    }
  }
}

private object CommandWebServiceTest {
  private val TestCommand = Terminate(sigkillProcessesAfter = Some(999.seconds))
}
