package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.core.command.CommandMeta
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.web.master.api.test.RouteTester
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class CommandRouteTest extends FreeSpec with RouteTester with CommandRoute {

  protected val masterId = MasterId("TEST-MASTER")
  protected implicit def scheduler: Scheduler = Scheduler.global

  private var commandReceived = false

  protected def executeCommand(command: MasterCommand, meta: CommandMeta) =
    command match {
      case MasterCommand.ShutDown(false) =>
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
