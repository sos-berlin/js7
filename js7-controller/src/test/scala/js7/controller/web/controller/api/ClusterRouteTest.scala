package js7.controller.web.controller.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.common.http.CirceJsonSupport._
import js7.controller.web.controller.api.test.RouteTester
import js7.data.cluster.ClusterState
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

  "ClusterRoute" in {
    Get("/cluster") ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[ClusterState] == ClusterState.Empty)
    }
  }
}
