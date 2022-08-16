package js7.controller.web.controller.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.test.OurTestSuite
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.common.akkahttp.CirceJsonSupport.jsonUnmarshaller
import js7.controller.web.controller.api.test.RouteTester
import js7.data.cluster.{ClusterNodeState, ClusterState}
import js7.data.node.NodeId
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class ClusterRouteTest extends OurTestSuite with RouteTester with ClusterRoute
{
  coupleScribeWithSlf4j()

  protected def scheduler = Scheduler.traced

  protected def whenShuttingDown = Future.never

  protected val nodeId = NodeId("NODE-ID")
  protected def clusterNodeIsBackup = false
  protected val checkedClusterState = Task.pure(Right(ClusterState.Empty))

  private lazy val route: Route =
    pathSegment("cluster") {
      clusterRoute
    }

  "/cluster" in {
    Get("/cluster") ~> Accept(`application/json`) ~> route ~> check {
      assert(status == OK && responseAs[ClusterState] == ClusterState.Empty)
    }
  }

  "/cluster?return=ClusterNodeState" in {
    Get("/cluster?return=ClusterNodeState") ~> Accept(`application/json`) ~> route ~> check {
      assert(status == OK && responseAs[ClusterNodeState] == ClusterNodeState(nodeId, false, ClusterState.Empty))
    }
  }
}
