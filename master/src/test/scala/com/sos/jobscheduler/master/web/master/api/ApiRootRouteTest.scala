package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.BuildInfo
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegment
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.core.filebased.Repo
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.data.event.EventId
import com.sos.jobscheduler.data.master.{MasterFileBaseds, MasterId}
import com.sos.jobscheduler.master.MasterState
import com.sos.jobscheduler.master.data.MasterOverview
import com.sos.jobscheduler.master.data.MasterSnapshots.MasterMetaState
import com.sos.jobscheduler.master.web.master.api.test.RouteTester
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class ApiRootRouteTest extends FreeSpec with RouteTester with ApiRootRoute
{
  protected val masterId = MasterId("TEST-MASTER")
  protected implicit def scheduler: Scheduler = Scheduler.global
  protected def masterState = Task.pure(MasterState(
    EventId(1001),
    MasterMetaState(MasterId("MASTER-ID"), Timestamp("2019-05-24T12:00:00Z")),
    ClusterState.Empty,
    Repo(MasterFileBaseds.jsonCodec),
    Map.empty,
    Map.empty))
  protected def totalRunningTime = Task.pure(1.hour)

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
      assert(overview.startedAt == Timestamp("2019-05-24T12:00:00Z"))
      assert(overview.totalRunningTime == 1.hour)
      assert(overview.orderCount == 0)
    }
  }
}
