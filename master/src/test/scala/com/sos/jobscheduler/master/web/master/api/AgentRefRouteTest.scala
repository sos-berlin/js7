package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.core.filebased.FileBasedApi
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.data.event.Stamped
import com.sos.jobscheduler.data.filebased.FileBasedsOverview
import com.sos.jobscheduler.master.web.master.api.AgentRefRouteTest._
import com.sos.jobscheduler.master.web.master.api.test.RouteTester
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class AgentRefRouteTest extends FreeSpec with RouteTester with AgentRefRoute {

  protected implicit def scheduler: Scheduler = Scheduler.global
  protected val fileBasedApi = FileBasedApi.forTest(pathToAgent)

  private def route: Route =
    pathSegments("api/agent") {
      agentRefRoute
    }

  // FileBasedsOverview
  AgentUri in {
    Get(AgentUri) ~> Accept(`application/json`) ~> route ~> check {
      assert(status == OK)
      assert(responseAs[FileBasedsOverview.Standard] == FileBasedsOverview.Standard(count = pathToAgent.size))
    }
  }

  // Seq[AgentsOverview]
  for (uri ← List(
      s"$AgentUri/")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        val Stamped(_, _, agents) = responseAs[Stamped[Seq[AgentRefPath]]]
        assert(agents == pathToAgent.keys.toList)
      }
    }
  }

  // Seq[AgentRef]
  for (uri ← List(
       s"$AgentUri/?return=AgentRef")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        val Stamped(_, _, agents) = responseAs[Stamped[Seq[AgentRef]]]
        assert(agents == pathToAgent.values.toVector)
      }
    }
  }

  // AgentRef
  for (uri ← List(
       s"$AgentUri/${pathToAgent.values.head.path.withoutStartingSlash}",
       s"$AgentUri/${pathToAgent.values.head.path.withoutStartingSlash.replace("/", "%2F")}")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[AgentRef] == pathToAgent.values.head)
      }
      Get(s"$uri?return=AgentRef") ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[AgentRef] == pathToAgent.values.head)
      }
    }
  }
}

object AgentRefRouteTest {
  private val AgentUri = "/api/agent"
  private val TestAgent = AgentRef(AgentRefPath("/PATH/AGENT") % "VERSION", "https://localhost:65535")
  private val pathToAgent = Map(TestAgent.path → TestAgent)
}
