package js7.controller.web.controller.api

import akka.http.scaladsl.server.Directives._
import js7.base.auth.ValidUserPermission
import js7.common.akkahttp.CirceJsonOrYamlSupport._
import js7.controller.web.common.ControllerRouteProvider
import js7.data.cluster.ClusterState
import monix.eval.Task

trait ClusterRoute extends ControllerRouteProvider
{
  private implicit def implicitScheduler = scheduler
  protected def clusterState: Task[ClusterState]

  protected final lazy val clusterRoute =
    authorizedUser(ValidUserPermission) { _ =>
      get {
        pathEnd {
          complete(clusterState.runToFuture)
        }
      }
    }
}
