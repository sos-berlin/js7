package js7.controller.web.controller.api.workflow

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import js7.base.problem.Checked
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.akkahttp.CirceJsonSupport.jsonUnmarshaller
import js7.controller.web.controller.api.test.RouteTester
import js7.controller.web.controller.api.workflow.WorkflowRouteTest._
import js7.core.item.VersionedItemApi
import js7.data.item.VersionedItemOverview
import js7.data.workflow.test.ForkTestSetting
import js7.data.workflow.{Workflow, WorkflowPath}
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class WorkflowRouteTest extends AnyFreeSpec with RouteTester with WorkflowRoute
{
  protected def whenShuttingDown = Future.never
  protected implicit def scheduler: Scheduler = Scheduler.global
  protected val itemApi = VersionedItemApi.forTest(pathToWorkflow)

  private def route: Route =
    pathSegments("api/workflow") {
      workflowRoute
    }

  // WorkflowsOverview
  WorkflowUri in {
    Get(WorkflowUri) ~> Accept(`application/json`) ~> route ~> check {
      assert(status == OK)
      assert(responseAs[VersionedItemOverview.Standard] == VersionedItemOverview.Standard(count = pathToWorkflow.size))
    }
  }

  // Seq[WorkflowsOverview]
  for (uri <- List(
      s"$WorkflowUri/")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[Checked[Seq[WorkflowPath]]] == Right(pathToWorkflow.keys.toList))
      }
    }
  }

  // Seq[Workflow]
  for (uri <- List(
       s"$WorkflowUri/?return=Workflow")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[Checked[Seq[Workflow]]] == Right(pathToWorkflow.values.toList))
      }
    }
  }

  // Workflow
  for (uri <- List(
       s"$WorkflowUri/${pathToWorkflow.values.head.path.string}",
       s"$WorkflowUri/${pathToWorkflow.values.head.path.string.replace("/", "%2F")}")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[Workflow] == pathToWorkflow.values.head)
      }
      Get(s"$uri?return=Workflow") ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        assert(responseAs[Workflow] == pathToWorkflow.values.head)
      }
    }
  }
}

object WorkflowRouteTest {
  private val WorkflowUri = "/api/workflow"
  private val TestWorkflow = ForkTestSetting.TestWorkflow.withId(WorkflowPath("PATH/WORKFLOW") ~ "VERSION")
  private val pathToWorkflow = Map(TestWorkflow.path -> TestWorkflow)
}
