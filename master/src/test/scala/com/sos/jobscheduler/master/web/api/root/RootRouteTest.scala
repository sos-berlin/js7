package com.sos.jobscheduler.master.web.api.root

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.common.CirceJsonSupport._
import com.sos.jobscheduler.common.akkahttp.AkkaHttpUtils.pathSegments
import com.sos.jobscheduler.master.command.MasterCommand
import com.sos.jobscheduler.master.data.MasterOverview
import com.sos.jobscheduler.master.web.simplegui.MasterWebServiceContext
import org.scalatest.FreeSpec
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class RootRouteTest extends FreeSpec with ScalatestRouteTest with RootRoute {

  protected implicit def executionContext = system.dispatcher
  protected val webServiceContext = new MasterWebServiceContext

  protected def executeCommand(command: MasterCommand) =
    command match {
      case MasterCommand.Terminate ⇒ Future.successful(MasterCommand.Response.Accepted)
    }

  protected def orderCountFuture = Future.successful(7)

  private def route: Route =
    pathSegments("api") {
      rootRoute
    }

  "/api" in {
    Get("/api") ~> Accept(`application/json`) ~> route ~> check {
      val overview = responseAs[MasterOverview]
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

object RootRouteTest {
  intelliJuseImport(jsonUnmarshaller)
}
