package js7.controller.web.controller.api

import cats.effect.{Deferred, IO}
import js7.base.problem.Checked
import js7.base.test.OurTestSuite
import js7.common.pekkohttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.pekkohttp.JsonString
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegments
import js7.controller.web.controller.api.test.RouteTester
import js7.core.command.CommandMeta
import js7.data.controller.{ControllerCommand, ControllerId}
import org.apache.pekko.http.scaladsl.model.MediaTypes.`application/json`
import org.apache.pekko.http.scaladsl.model.StatusCodes.ContentTooLarge
import org.apache.pekko.http.scaladsl.model.headers.Accept
import org.apache.pekko.http.scaladsl.server.Route

/**
  * @author Joacim Zschimmer
  */
final class CommandRouteTest extends OurTestSuite, RouteTester, CommandRoute:

  protected val controllerId = ControllerId("TEST-CONTROLLER")
  protected def whenShuttingDown = Deferred.unsafe

  private var commandReceived = false

  protected def executeCommand(command: ControllerCommand, meta: CommandMeta): IO[Checked[command.Response]] =
    command match
      case ControllerCommand.NoOperation(None) =>
        commandReceived = true
        IO.pure(Right(ControllerCommand.Response.Accepted.asInstanceOf[command.Response]))

      case _ =>
        fail()

  private def route: Route =
    pathSegments("api/command"):
      commandRoute

  "POST /api/command" in:
    assert(!commandReceived)
    Post("/api/command", ControllerCommand.NoOperation(): ControllerCommand) ~>
      Accept(`application/json`) ~>
      route ~> check:
        val response = responseAs[ControllerCommand.Response]
        assert(response == ControllerCommand.Response.Accepted)
    assert(commandReceived)

  "js7.web.server.services.command-size-limit" - {
    lazy val entitySizeLimit =
      config.getMemorySize("js7.web.server.services.command-size-limit").toBytes

    "POST /api/command, big JSON" in:
      testBigJson(entitySizeLimit.toInt):
        val response = responseAs[ControllerCommand.Response]
        assert(response == ControllerCommand.Response.Accepted)

    "POST /api/command, too big JSON is rejected with 412 Payload to large" in:
      testBigJson(entitySizeLimit.toInt + 1):
        assert(status == ContentTooLarge)

    def testBigJson(size: Int)(checkBody: => Unit): Unit =
      var json = """{ "TYPE": "NoOperation" """
      json += " " * (size - json.length - 1) + "}"
      Post("/api/command", JsonString(json)) ~> Accept(`application/json`) ~>
        route ~> check(checkBody)
  }
