package com.sos.jobscheduler.master.web.master.api.workflow

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.filebased.FileBasedApi
import com.sos.jobscheduler.data.event.Stamped
import com.sos.jobscheduler.data.filebased.FileBasedsOverview
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.web.master.api.test.RouteTester
import com.sos.jobscheduler.master.web.master.api.workflow.WorkflowRouteTest._
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final class WorkflowRouteTest extends FreeSpec with RouteTester with WorkflowRoute {

  protected implicit def scheduler = Scheduler.global
  protected val fileBasedApi = FileBasedApi.forTest(pathToWorkflow)
  private implicit val timerService = new TimerService(idleTimeout = Some(1.s))
  protected val eventReader = new EventCollector.ForTest

  private def route: Route =
    pathSegments("api/workflow") {
      workflowRoute
    }

  // WorkflowsOverview
  WorkflowUri in {
    Get(WorkflowUri) ~> Accept(`application/json`) ~> route ~> check {
      assert(status == OK)
      assert(responseAs[FileBasedsOverview.Standard] == FileBasedsOverview.Standard(count = pathToWorkflow.size))
    }
  }

  // Seq[WorkflowsOverview]
  for (uri ← List(
      s"$WorkflowUri/")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        val Stamped(_, _, workflows) = responseAs[Stamped[Seq[WorkflowPath]]]
        assert(workflows == pathToWorkflow.keys.toList)
      }
    }
  }

  // Seq[Workflow]
  for (uri ← List(
       s"$WorkflowUri/?return=Workflow")) {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        assert(status == OK)
        val Stamped(_, _, workflows) = responseAs[Stamped[Seq[Workflow]]]
        assert(workflows == pathToWorkflow.values.toVector)
      }
    }
  }

  // Workflow
  for (uri ← List(
       s"$WorkflowUri/${pathToWorkflow.values.head.path.withoutStartingSlash}",
       s"$WorkflowUri/${pathToWorkflow.values.head.path.withoutStartingSlash.replace("/", "%2F")}")) {
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
  private val TestWorkflow = ForkTestSetting.TestWorkflow.withId(WorkflowPath("/PATH/WORKFLOW") % "VERSION")
  private val pathToWorkflow = Map(TestWorkflow.path → TestWorkflow)
}
