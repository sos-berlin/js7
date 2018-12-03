package com.sos.jobscheduler.agent.web

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{OK, ServiceUnavailable}
import akka.http.scaladsl.model.headers.Accept
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand._
import com.sos.jobscheduler.agent.scheduler.problems.AgentIsShuttingDownProblem
import com.sos.jobscheduler.agent.web.CommandWebServiceTest._
import com.sos.jobscheduler.agent.web.test.WebServiceTest
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.data.command.{CommandHandlerDetailed, CommandHandlerOverview, CommandRunOverview, InternalCommandId}
import io.circe.Json
import io.circe.syntax.EncoderOps
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
 * @author Joacim Zschimmer
 */
final class CommandWebServiceTest extends FreeSpec with WebServiceTest with CommandWebService {

  protected def scheduler = Scheduler.global
  override protected val uriPathPrefix = "test"

  protected def commandExecute(meta: CommandMeta, command: AgentCommand) =
    Task(
      command match {
        case TestCommand ⇒ AgentCommand.Response.Accepted
        case TestCommandWhileShuttingDown ⇒ throw AgentIsShuttingDownProblem.throwable
        case _ ⇒ fail()
      })

  protected def commandOverview = Task.pure(CommandHandlerOverview(currentCommandCount = 111, totalCommandCount = 222))

  protected def commandDetailed = Task.pure(CommandHandlerDetailed(List(
    CommandRunOverview(InternalCommandId(333), Timestamp("2015-06-22T12:00:00Z"), TestCommand))))

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
      assert(responseAs[AgentCommand.Response.Accepted] == AgentCommand.Response.Accepted)
      assert(responseEntity.toStrict(9.seconds).value.get.get.data.utf8String.parseJson ==
        """{ "TYPE": "Accepted" }""".parseJson)
    }
  }

  "Command while shutting down return 503 Service Unavailable" in {
    // When Agent is shutting down, the command may be okay and the Master should repeat the command later
    // Not valid for commands packaged in AgentCommand.Batch
    postJsonCommand((TestCommandWhileShuttingDown: AgentCommand).asJson) ~> check {
      if (status != ServiceUnavailable) fail(s"$status - ${responseEntity.toStrict(9.seconds).value}")
      assert(responseAs[AgentCommand.Response.Accepted] == AgentCommand.Response.Accepted)
      assert(responseEntity.toStrict(9.seconds).value.get.get.data.utf8String.parseJson ==
        """{
          "TYPE": "Problem",
          "message": "Agent is shutting down"
         }""".parseJson)
    }
  }

  private def postJsonCommand(json: Json): RouteTestResult =
    Post("/agent/api/command", json) ~>
      testSessionHeader ~>
      Accept(`application/json`) ~>
      route

  "commandHandler returns overview" in {
    Get("/agent/api/command") ~> testSessionHeader ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[Json] == Json.obj(
        "currentCommandCount" → Json.fromInt(111),
        "totalCommandCount" → Json.fromInt(222)))
    }
  }

  "commandHandler/ returns array of running command" in {
    Get("/agent/api/command/") ~> testSessionHeader ~> Accept(`application/json`) ~> route ~> check {
      assert(status == OK)
      assert(responseAs[Json] == Json.fromValues(List(
        Json.obj(
          "internalId" → "333".asJson,
          "startedAt" → Timestamp("2015-06-22T12:00:00Z").toEpochMilli.asJson,
          "command" → (TestCommand: AgentCommand).asJson))))
    }
  }
}

private object CommandWebServiceTest {
  private val TestCommand = Terminate(sigkillProcessesAfter = Some(999.seconds))
  private val TestCommandWhileShuttingDown = Terminate(sigkillProcessesAfter = Some(777.seconds))
}
