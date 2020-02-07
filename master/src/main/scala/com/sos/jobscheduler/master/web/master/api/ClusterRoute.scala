package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.server.Directives._
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.http.CirceJsonSupport.jsonMarshaller
import com.sos.jobscheduler.data.cluster.ClusterState
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import monix.eval.Task

trait ClusterRoute extends MasterRouteProvider
{
  private implicit def implicitScheduler = scheduler
  protected def clusterState: Task[Checked[ClusterState]]

  protected final lazy val clusterRoute =
    get {
      pathEnd {
        complete(clusterState.runToFuture)
      }
    }
}
