package js7.controller.web.controller.api

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import js7.base.problem.Checked
import js7.base.utils.Collections.implicits._
import js7.base.web.Uri
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.http.CirceJsonSupport._
import js7.controller.data.agent.AgentRefState
import js7.controller.web.controller.api.AgentRefRouteTest._
import js7.controller.web.controller.api.test.RouteTester
import js7.data.agent.{AgentName, AgentRef}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class AgentRefRouteTest extends AnyFreeSpec with RouteTester with AgentRefRoute
{
  protected def whenShuttingDown = Future.never
  protected implicit def scheduler: Scheduler = Scheduler.global
  protected val nameToAgentRefState = Task { Right(nameToAgent) }

  private def route: Route =
    pathSegments("api/agent") {
      agentRefRoute
    }

  // InventoryItemOverview
  //AgentUri in {
  //  Get(AgentUri) ~> Accept(`application/json`) ~> route ~> check {
  //    assert(status == OK)
  //    assert(responseAs[InventoryItemOverview.Standard] == InventoryItemOverview.Standard(count = nameToAgent.size))
  //  }
  //}

  // Seq[AgentName]
  for (uri <- List(s"$AgentUri/")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[Checked[Set[AgentName]]] == Right(nameToAgent.keySet))
      }
    }
  }

  for (uri <- List(s"$AgentUri/?return=AgentRef")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[Set[AgentRef]] == nameToAgent.values.map(_.agentRef).toSet)
      }
    }
  }

  for (uri <- List(s"$AgentUri/?return=AgentRefState")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[Checked[Set[AgentRefState]]] == Right(nameToAgent.values.toSet))
      }
    }
  }

  // AgentRef
  for (uri <- List(s"$AgentUri/${nameToAgent.values.head.name}")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[AgentRefState] == nameToAgent.values.head)
      }
    }
  }
}

object AgentRefRouteTest
{
  private val AgentUri = "/api/agent"
  private val aAgent = AgentRef(AgentName("A-AGENT"), Uri("https://localhost:0"))
  private val bAgent = AgentRef(AgentName("B-AGENT"), Uri("https://localhost:65535"))
  private val nameToAgent = Seq(aAgent, bAgent).map(AgentRefState.apply).toKeyedMap(_.name)
}
