package js7.core.cluster

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{BadRequest, OK}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import io.circe.JsonObject
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.web.Uri
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.common.akkahttp.web.session.SimpleSession
import js7.common.http.CirceJsonSupport._
import js7.data.cluster.ClusterEvent.ClusterNodesAppointed
import js7.data.cluster.{ClusterSetting, ClusterState}
import js7.data.controller.ControllerId
import js7.data.node.NodeId
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ControllersClusterRouteTest extends AnyFreeSpec with ScalatestRouteTest with ControllersClusterRoute
{
  protected type Session = SimpleSession

  protected implicit val scheduler = Scheduler.global
  protected val clusterWatchRegister = new ClusterWatchRegister(scheduler)

  private implicit val routeTestTimeout = RouteTestTimeout(10.s)
  private val controllerId = ControllerId("CONTROLLER")

  private val route = pathSegment("cluster") {
    controllerClusterRoute(controllerId)
  }

  "Get for unknown ControllerId" in {
    Get("/cluster") ~> Accept(`application/json`) ~> route ~> check {
      assert(status == BadRequest &&
        entityAs[Problem] == Problem("No ClusterState registered for ControllerId 'CONTROLLER'"))
    }
  }

  private val setting = ClusterSetting(
    Map(
      NodeId("A") -> Uri("http://A"),
      NodeId("B") -> Uri("http://B")),
    NodeId("A"))

  "Post" in {
    Post[ClusterWatchMessage]("/cluster",
      ClusterWatchEvents(
        NodeId("A"),
        Seq(ClusterNodesAppointed(setting)),
        ClusterState.NodesAppointed(setting))
    ) ~>
      Accept(`application/json`) ~> route ~>
      check {
        assert(status == OK && entityAs[JsonObject] == JsonObject.empty)
      }
  }

  "Get for known ControllerId" in {
    Get("/cluster") ~> Accept(`application/json`) ~> route ~> check {
      assert(status == OK &&
        entityAs[ClusterState] == ClusterState.NodesAppointed(setting))
    }
  }
}
