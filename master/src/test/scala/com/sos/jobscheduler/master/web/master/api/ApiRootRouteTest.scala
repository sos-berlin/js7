package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegment
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.master.data.MasterOverview
import com.sos.jobscheduler.master.web.master.api.test.RouteTester
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ApiRootRouteTest extends FreeSpec with RouteTester with ApiRootRoute {

  protected val masterId = MasterId("TEST-MASTER")
  protected implicit def scheduler: Scheduler = Scheduler.global

  protected def orderCount = Task.pure(7)

  private def route: Route =
    pathSegment("api") {
      apiRootRoute
    }

  "/api" in {
    Get("/api") ~> Accept(`application/json`) ~> route ~> check {
      val overview = responseAs[MasterOverview]
      assert(overview.id == masterId)
      assert(overview.version == BuildInfo.prettyVersion)
      assert(overview.buildId == BuildInfo.buildId)
      assert(overview.java.systemProperties("java.version") == sys.props("java.version"))
      assert(overview.orderCount == 7)
    }
  }
}
