package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.server.Directives._
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.common.akkahttp.CirceJsonOrYamlSupport._
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import monix.eval.Task

trait ClusterRoute extends MasterRouteProvider
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
