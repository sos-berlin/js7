package js7.master.web.master.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import js7.base.problem.Checked
import js7.base.web.Uri
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.http.CirceJsonSupport._
import js7.core.filebased.FileBasedApi
import js7.data.agent.{AgentRef, AgentRefPath}
import js7.data.filebased.FileBasedsOverview
import js7.master.web.master.api.AgentRefRouteTest._
import js7.master.web.master.api.test.RouteTester
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AgentRefRouteTest extends AnyFreeSpec with RouteTester with AgentRefRoute
{
  protected def isShuttingDown = false
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
  for (uri <- List(
      s"$AgentUri/")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[Checked[Seq[AgentRefPath]]] == Right(pathToAgent.keys.toList))
      }
    }
  }

  // Seq[AgentRef]
  for (uri <- List(
       s"$AgentUri/?return=AgentRef")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[Checked[Seq[AgentRef]]] == Right(pathToAgent.values.toVector))
      }
    }
  }

  // AgentRef
  for (uri <- List(
       s"$AgentUri/${pathToAgent.values.head.path.withoutStartingSlash}",
       s"$AgentUri/${pathToAgent.values.head.path.string}",
       s"$AgentUri/${pathToAgent.values.head.path.string.replace("/", "%2F")}")) {
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
  private val TestAgent = AgentRef(AgentRefPath("/PATH/AGENT") ~ "VERSION", Uri("https://localhost:65535"))
  private val pathToAgent = Map(TestAgent.path -> TestAgent)
}
