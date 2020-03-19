package com.sos.jobscheduler.core.cluster

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{BadRequest, OK}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegment
import com.sos.jobscheduler.common.akkahttp.web.session.SimpleSession
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.data.cluster.ClusterState.ClusterSole
import com.sos.jobscheduler.data.cluster.{ClusterEvent, ClusterState}
import com.sos.jobscheduler.data.common.Uri
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
  private val fromUri = Uri("http://example.com")

  private val route = pathSegment("cluster") {
    masterClusterRoute(masterId)
  }

  "Get for unknown MasterId" in {
    Get("/cluster") ~> Accept(`application/json`) ~> route ~> check {
      assert(status == BadRequest && entityAs[Problem] == Problem("No ClusterState registered for MasterId 'MASTER'"))
    }
  }

  "Post" in {
    Post[ClusterWatchMessage]("/cluster", ClusterWatchEvents(fromUri, ClusterEvent.BecameSole(fromUri) :: Nil, ClusterSole(fromUri))) ~>
      Accept(`application/json`) ~> route ~>
      check {
        assert(status == OK && entityAs[JsonObject] == JsonObject.empty)
      }
  }

  "Get for known MasterId" in {
    Get("/cluster") ~> Accept(`application/json`) ~> route ~> check {
      assert(status == OK && entityAs[ClusterState] == ClusterSole(fromUri))
    }
  }
}
