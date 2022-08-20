package js7.core.cluster.watch

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{BadRequest, OK}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import io.circe.JsonObject
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.web.Uri
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.common.akkahttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.data.cluster.ClusterEvent.ClusterNodesAppointed
import js7.data.cluster.{ClusterSetting, ClusterState, ClusterTiming}
import js7.data.controller.ControllerId
import js7.data.node.NodeId
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
final class ClusterWatchRouteTest extends OurTestSuite with ScalatestRouteTest with ClusterWatchRoute
{
  coupleScribeWithSlf4j()

  protected implicit val scheduler = Scheduler.traced
  protected val clusterWatchRegister = new ClusterWatchRegister(scheduler)

  private implicit val routeTestTimeout: RouteTestTimeout = RouteTestTimeout(10.s)
  private val controllerId = ControllerId("CONTROLLER")

  private val route = pathSegment("cluster") {
    clusterWatchRoute(controllerId)
  }

  "Get for unknown ControllerId" in {
    Get("/cluster") ~> Accept(`application/json`) ~> route ~> check {
      assert(status == BadRequest &&
        entityAs[Problem] == Problem("No ClusterState registered for 'ControllerId:CONTROLLER'"))
    }
  }

  private val setting = ClusterSetting(
    Map(
      NodeId("A") -> Uri("https://A"),
      NodeId("B") -> Uri("https://B")),
    NodeId("A"),
    Seq(ClusterSetting.Watch(Uri("https://CLUSTER-WATCH"))),
    ClusterTiming(10.s, 20.s))

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