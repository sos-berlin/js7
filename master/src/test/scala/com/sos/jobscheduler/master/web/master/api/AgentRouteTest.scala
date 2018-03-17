package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.sos.jobscheduler.base.utils.ScalaUtils.RichPartialFunction
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.Stamped
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedsOverview}
import com.sos.jobscheduler.master.FileBasedApi
import com.sos.jobscheduler.master.order.agent.Agent
import com.sos.jobscheduler.master.web.master.api.AgentRouteTest._
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class AgentRouteTest extends FreeSpec with ScalatestRouteTest with AgentRoute {

  protected implicit def scheduler = Scheduler.global
  protected val fileBasedApi = new FileBasedApi {
    def overview[A <: FileBased: FileBased.Companion] =
      Task.now(Stamped(1, FileBasedsOverview(count = pathToAgent.values.size)))

    def fileBaseds[A <: FileBased: FileBased.Companion] =
      Task.now(Stamped(2, pathToAgent.values.toVector map (_.cast[A])))

    def fileBased[A <: FileBased: FileBased.Companion](path: A#Path) =
      Task.now(
        for (a ← pathToAgent.checked(path.cast[AgentPath]))
          yield Stamped(3, a.cast[A]))
  }

  private def route: Route =
    pathSegments("api/agent") {
      agentRoute
    }

  // FileBasedsOverview
  AgentUri in {
    Get(AgentUri) ~> Accept(`application/json`) ~> route ~> check {
      assert(status == OK)
      assert(responseAs[FileBasedsOverview] == FileBasedsOverview(count = pathToAgent.size))
    }
  }

  // Seq[AgentOverview]
  for (uri ← List(
      s"$AgentUri/")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        val Stamped(_, _, agents) = responseAs[Stamped[Seq[AgentPath]]]
        assert(agents == (pathToAgent.keys.toList))
      }
    }
  }

  // Seq[Agent]
  for (uri ← List(
       s"$AgentUri/?return=Agent")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        val Stamped(_, _, agents) = responseAs[Stamped[Seq[Agent]]]
        assert(agents == pathToAgent.values.toVector)
      }
    }
  }

  // Agent
  for (uri ← List(
       s"$AgentUri/${pathToAgent.values.head.path.withoutStartingSlash}",
       s"$AgentUri/${pathToAgent.values.head.path.withoutStartingSlash.replace("/", "%2F")}")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[Agent] == pathToAgent.values.head)
      }
      Get(s"$uri?return=Agent") ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[Agent] == pathToAgent.values.head)
      }
    }
  }
}

object AgentRouteTest {
  private val AgentUri = "/api/agent"
  private val TestAgent = Agent(AgentPath("/PATH/AGENT") % "VERSION", "https://localhost:65535")
  private val pathToAgent = Map(TestAgent.path → TestAgent)
}
