package js7.controller.web.controller.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.common.http.CirceJsonSupport._
import js7.controller.web.controller.api.test.RouteTester
import js7.data.cluster.{ClusterState, ExtendedClusterState}
import js7.data.node.NodeId
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class ClusterRouteTest extends AnyFreeSpec with RouteTester with ClusterRoute
{
  protected def scheduler = Scheduler.global

  protected def whenShuttingDown = Future.never

  protected val nodeId = NodeId("NODE-ID")
  protected val checkedClusterState = Task.pure(Right(ClusterState.Empty))

  private lazy val route: Route =
    pathSegment("cluster") {
      clusterRoute
    }

  "/cluster" in {
    Get("/cluster") ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[ClusterState] == ClusterState.Empty)
    }
  }

  "/cluster?return=ExtendedClusterState" in {
    Get("/cluster?return=ExtendedClusterState") ~> Accept(`application/json`) ~> route ~> check {
      assert(responseAs[ExtendedClusterState] == ExtendedClusterState(nodeId, ClusterState.Empty))
    }
  }
}
