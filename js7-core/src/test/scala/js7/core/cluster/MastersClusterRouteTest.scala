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
import js7.data.cluster.{ClusterNodeId, ClusterState}
import js7.data.master.MasterId
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MastersClusterRouteTest extends AnyFreeSpec with ScalatestRouteTest with MastersClusterRoute
{
  protected type Session = SimpleSession

  protected implicit val scheduler = Scheduler.global
  protected val clusterWatchRegister = new ClusterWatchRegister(scheduler)

  private implicit val routeTestTimeout = RouteTestTimeout(10.s)
  private val masterId = MasterId("MASTER")

  private val route = pathSegment("cluster") {
    masterClusterRoute(masterId)
  }

  "Get for unknown MasterId" in {
    Get("/cluster") ~> Accept(`application/json`) ~> route ~> check {
      assert(status == BadRequest &&
        entityAs[Problem] == Problem("No ClusterState registered for MasterId 'MASTER'"))
    }
  }

  private val idToUri = Map(
    ClusterNodeId("A") -> Uri("http://A"),
    ClusterNodeId("B") -> Uri("http://B"))
  private val primaryId = ClusterNodeId("A")

  "Post" in {
    Post[ClusterWatchMessage]("/cluster",
      ClusterWatchEvents(
        ClusterNodeId("A"),
        ClusterNodesAppointed(idToUri, primaryId) :: Nil,
        ClusterState.NodesAppointed(idToUri, primaryId))
    ) ~>
      Accept(`application/json`) ~> route ~>
      check {
        assert(status == OK && entityAs[JsonObject] == JsonObject.empty)
      }
  }

  "Get for known MasterId" in {
    Get("/cluster") ~> Accept(`application/json`) ~> route ~> check {
      assert(status == OK &&
        entityAs[ClusterState] == ClusterState.NodesAppointed(idToUri, primaryId))
    }
  }
}
