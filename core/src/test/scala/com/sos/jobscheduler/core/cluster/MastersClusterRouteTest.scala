package com.sos.jobscheduler.core.cluster

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{BadRequest, OK}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegment
import com.sos.jobscheduler.common.akkahttp.web.session.SimpleSession
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.data.cluster.ClusterEvent.ClusterNodesAppointed
import com.sos.jobscheduler.data.cluster.{ClusterNodeId, ClusterState}
import com.sos.jobscheduler.data.master.MasterId
import io.circe.JsonObject
import monix.execution.Scheduler
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MastersClusterRouteTest extends FreeSpec with ScalatestRouteTest with MastersClusterRoute
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
