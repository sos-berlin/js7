package com.sos.jobscheduler.master.web.master.api.workflow

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.WorkflowClient
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
trait WorkflowRoute {

  protected def workflowClient: WorkflowClient
  protected implicit def executionContext: ExecutionContext

  def workflowRoute: Route =
    get {
      pathEnd {
        complete(workflowClient.workflowsOverview)
      } ~
      pathSingleSlash {
        parameter("return".?) {
          case Some("WorkflowOverview") | None ⇒
            complete(workflowClient.workflowOverviews)

          case Some("Workflow") | None ⇒
            complete(workflowClient.workflows)

          case _ ⇒
            reject
        }
      } ~
      path(Segment) { pathString ⇒
        singleWorkflow(WorkflowPath(s"/$pathString"))
      } ~
      extractUnmatchedPath {
        case Uri.Path.Slash(tail) if !tail.isEmpty ⇒ singleWorkflow(WorkflowPath("/" + tail.toString))  // Slashes not escaped
        case _ ⇒ reject
      }
    }

  private def singleWorkflow(path: WorkflowPath): Route =
    complete {
      workflowClient.workflow(path) map {
        case Some(o) ⇒
          o: ToResponseMarshallable
        case None ⇒
          BadRequest → s"Does not exist: $path\n": ToResponseMarshallable
      }
    }
}
