package js7.master.web.master.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.http.CirceJsonSupport._
import js7.core.command.CommandMeta
import js7.data.master.MasterId
import js7.master.data.MasterCommand
import js7.master.web.master.api.test.RouteTester
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CommandRouteTest extends AnyFreeSpec with RouteTester with CommandRoute {

  protected val masterId = MasterId("TEST-MASTER")
  protected def isShuttingDown = false
  protected implicit def scheduler: Scheduler = Scheduler.global

  private var commandReceived = false

  protected def executeCommand(command: MasterCommand, meta: CommandMeta) =
    command match {
      case shutDown: MasterCommand.ShutDown if !shutDown.restart =>
        commandReceived = true
        Task.pure(Right(MasterCommand.Response.Accepted))

      case _ =>
        fail()
    }

  private def route: Route =
    pathSegments("api/command") {
      commandRoute
    }

  "POST /api/command" in {
    assert(!commandReceived)
    Post("/api/command", MasterCommand.ShutDown(): MasterCommand) ~> Accept(`application/json`) ~> route ~> check {
      val response = responseAs[MasterCommand.Response]
      assert(response == MasterCommand.Response.Accepted)
    }
    assert(commandReceived)
  }
}
