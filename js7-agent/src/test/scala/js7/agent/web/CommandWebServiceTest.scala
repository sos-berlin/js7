package js7.agent.web

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{OK, ServiceUnavailable}
import akka.http.scaladsl.model.headers.Accept
import io.circe.Json
import io.circe.syntax.EncoderOps
import js7.agent.data.Problems.AgentIsShuttingDown
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand._
import js7.agent.web.CommandWebServiceTest._
import js7.agent.web.test.WebServiceTest
import js7.base.circeutils.CirceUtils._
import js7.base.circeutils.CirceUtils.implicits._
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.time.ScalaTime._
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.akkahttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.core.command.CommandMeta
import js7.data.command.{CommandHandlerDetailed, CommandHandlerOverview, CommandRunOverview, InternalCommandId}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class CommandWebServiceTest extends AnyFreeSpec with WebServiceTest with CommandWebService
{
  protected def whenShuttingDown = Future.never
  protected def scheduler = Scheduler.global
  override protected val uriPathPrefix = "test"

  protected def commandExecute(meta: CommandMeta, command: AgentCommand) =
    Task(
      command match {
        case TestCommand => Right(AgentCommand.Response.Accepted)
        case TestCommandWhileShuttingDown => Left(AgentIsShuttingDown)
        case _ => fail()
      })

  protected def commandOverview = Task.pure(CommandHandlerOverview(currentCommandCount = 111, totalCommandCount = 222))

  protected def commandDetailed = Task.pure(CommandHandlerDetailed(List(
    CommandRunOverview(InternalCommandId(333), 1.h, TestCommand))))

  private val route =
    pathSegments("agent/api/command") {
      commandRoute
    }

  "ShutDown" in {
    val json = json"""{ "TYPE": "ShutDown" }"""
    postJsonCommand(json) ~> check {
      if (status != OK) fail(s"$status - ${responseEntity.toStrict(99.s).value}")
      assert(responseAs[AgentCommand.Response.Accepted] == AgentCommand.Response.Accepted)
      assert(responseEntity.toStrict(99.s).value.get.get.data.utf8String.parseJsonOrThrow ==
        json"""{ "TYPE": "Accepted" }""")
    }
  }

  "Command while shutting down return 503 Service Unavailable" in {
    // When Agent is shutting down, the command may be okay and the Controller should repeat the command later
    // Not valid for commands packaged in AgentCommand.Batch
    postJsonCommand((TestCommandWhileShuttingDown: AgentCommand).asJson) ~> check {
      if (status != ServiceUnavailable) fail(s"$status - ${responseEntity.toStrict(99.s).value}")
      assert(responseAs[AgentCommand.Response.Accepted] == AgentCommand.Response.Accepted)
      assert(responseEntity.toStrict(99.s).value.get.get.data.utf8String.parseJsonOrThrow ==
        json"""{
          "TYPE": "Problem",
          "code": "AgentIsShuttingDown",
          "message": "Agent is shutting down"
         }""")
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
        "currentCommandCount" -> Json.fromInt(111),
        "totalCommandCount" -> Json.fromInt(222)))
    }
  }

  "commandHandler/ returns array of running commands" in {
    Get("/agent/api/command/") ~> testSessionHeader ~> Accept(`application/json`) ~> route ~> check {
      assert(status == OK)
      assert(responseAs[Json] == Json.fromValues(List(
        Json.obj(
          "internalId" -> "333".asJson,
          "duration" -> 3600.asJson,
          "command" -> (TestCommand: AgentCommand).asJson.dropNullValues))))
    }
  }
}

private object CommandWebServiceTest {
  private val TestCommand = ShutDown(None)
  private val TestCommandWhileShuttingDown = ShutDown(Some(SIGTERM))
}
