package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegment
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.master.web.master.api.test.RouteTester
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ClusterRouteTest extends AnyFreeSpec with RouteTester with ClusterRoute
{
  protected def scheduler = Scheduler.global

  protected def isShuttingDown = false

  protected def clusterState = Task.pure(ClusterState.Empty)

  private def route: Route =
    pathSegment("cluster") {
      clusterRoute
    }

  Get("/cluster") ~> Accept(`application/json`) ~> route ~> check {
    assert(responseAs[ClusterState] == ClusterState.Empty)
  }
}
