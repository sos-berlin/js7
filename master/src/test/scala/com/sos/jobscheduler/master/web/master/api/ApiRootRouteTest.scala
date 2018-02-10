package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.CirceJsonSupport._
import com.sos.jobscheduler.common.akkahttp.AkkaHttpUtils.pathSegments
import com.sos.jobscheduler.master.data.{MasterCommand, MasterOverview}
import org.scalatest.FreeSpec
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class ApiRootRouteTest extends FreeSpec with ScalatestRouteTest with ApiRootRoute {

  protected implicit def executionContext = system.dispatcher

  protected def executeCommand(command: MasterCommand) =
    command match {
      case MasterCommand.Terminate ⇒ Future.successful(MasterCommand.Response.Accepted)
    }

  protected def orderCountFuture = Future.successful(7)

  private def route: Route =
    pathSegments("api") {
      apiRootRoute
    }

  "/api" in {
    Get("/api") ~> Accept(`application/json`) ~> route ~> check {
      val overview = responseAs[MasterOverview]
      assert(overview.version == BuildInfo.buildVersion)
      assert(overview.buildId == BuildInfo.buildId)
      assert(overview.java.systemProperties("java.version") == sys.props("java.version"))
      assert(overview.orderCount == 7)
    }
  }

  "POST /api" in {
    Post("/api", MasterCommand.Terminate: MasterCommand) ~> route → check {
      val response = responseAs[MasterCommand.Response]
      assert(response == MasterCommand.Response.Accepted)
    }
  }
}
