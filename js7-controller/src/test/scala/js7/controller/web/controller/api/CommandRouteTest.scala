package js7.controller.web.controller.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import js7.base.problem.Checked
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.akkahttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
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
final class CommandRouteTest extends AnyFreeSpec with RouteTester with CommandRoute {

  protected val controllerId = ControllerId("TEST-CONTROLLER")
  protected def whenShuttingDown = Future.never
  protected implicit def scheduler: Scheduler = Scheduler.global

  private var commandReceived = false

  protected def executeCommand(command: ControllerCommand, meta: CommandMeta): Task[Checked[command.Response]] =
    command match {
      case shutDown: ControllerCommand.ShutDown if !shutDown.restart =>
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
    Post("/api/command", ControllerCommand.ShutDown(): ControllerCommand) ~> Accept(`application/json`) ~> route ~> check {
      val response = responseAs[ControllerCommand.Response]
      assert(response == ControllerCommand.Response.Accepted)
    }
    assert(commandReceived)
  }
}
