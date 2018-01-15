package com.sos.jobscheduler.master.web.master.api.workflow

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.data.workflow.{WorkflowPath, Workflow}
import com.sos.jobscheduler.master.WorkflowClient
import io.circe
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

          case Some("Workflow") | Some("Workflow") | None ⇒
            complete(workflowClient.namedWorkflows)

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
    parameter("return".?) {
      case Some("Workflow") | Some("Workflow") | None ⇒
        completeAs(path, _.workflow)

      case o ⇒
        complete((BadRequest → s"Unrecognized parameter return=$o\n"))
    }

  private def completeAs[A: circe.Encoder](path: WorkflowPath, as: Workflow.Named ⇒ A) =
    complete {
      workflowClient.namedWorkflow(path) map {
        case Some(o) ⇒
          as(o): ToResponseMarshallable
        case None ⇒
          BadRequest → s"$path does not exist\n": ToResponseMarshallable
      }
    }
}
