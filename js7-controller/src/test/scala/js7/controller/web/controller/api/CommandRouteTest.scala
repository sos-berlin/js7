package js7.controller.web.controller.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.PayloadTooLarge
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.problem.Checked
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.akkahttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.akkahttp.JsonString
import js7.controller.web.controller.api.test.RouteTester
import js7.core.command.CommandMeta
import js7.data.controller.{ControllerCommand, ControllerId}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class CommandRouteTest extends AnyFreeSpec with RouteTester with CommandRoute
{
  coupleScribeWithSlf4j()

  protected val controllerId = ControllerId("TEST-CONTROLLER")
  protected def whenShuttingDown = Future.never
  protected implicit def scheduler: Scheduler = Scheduler.global

  private var commandReceived = false

  protected def executeCommand(command: ControllerCommand, meta: CommandMeta): Task[Checked[command.Response]] =
    command match {
      case ControllerCommand.NoOperation(None) =>
        commandReceived = true
        Task.pure(Right(ControllerCommand.Response.Accepted.asInstanceOf[command.Response]))

      case _ =>
        fail()
    }

  private def route: Route =
    pathSegments("api/command") {
      commandRoute
    }

  "POST /api/command" in {
    assert(!commandReceived)
    Post("/api/command", ControllerCommand.NoOperation(): ControllerCommand) ~> Accept(`application/json`) ~> route ~> check {
      val response = responseAs[ControllerCommand.Response]
      assert(response == ControllerCommand.Response.Accepted)
    }
    assert(commandReceived)
  }

  "js7.web.server.services.command-size-limit" - {
    lazy val entitySizeLimit =
      config.getMemorySize("js7.web.server.services.command-size-limit").toBytes

    "POST /api/command, big JSON" in {
      testBigJson(entitySizeLimit.toInt) {
        val response = responseAs[ControllerCommand.Response]
        assert(response == ControllerCommand.Response.Accepted)
      }
    }

    "POST /api/command, too big JSON is rejected with 412 Payload to large" in {
      testBigJson(entitySizeLimit.toInt + 1) {
        assert(status == PayloadTooLarge)
      }
    }

    def testBigJson(size: Int)(checkBody: => Unit): Unit = {
      var json = """{ "TYPE": "NoOperation" """
      json += " " * (size - json.length - 1) + "}"
      Post("/api/command", JsonString(json)) ~> Accept(`application/json`) ~> route ~> check(checkBody)
    }
  }
}
