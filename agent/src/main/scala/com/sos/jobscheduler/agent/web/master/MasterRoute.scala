package com.sos.jobscheduler.agent.web.master

import akka.http.scaladsl.server.RouteConcatenation._
import com.sos.jobscheduler.base.auth.ValidUserPermission
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegment
import com.sos.jobscheduler.core.cluster.MastersClusterRoute
import com.sos.jobscheduler.data.master.MasterId

trait MasterRoute extends MastersEventRoute with MastersClusterRoute
{
  protected final lazy val masterRoute =
    pathSegment("event") {
      masterEventRoute
    } ~
    pathSegment("cluster") {
      authorizedUser(ValidUserPermission) { user =>
        masterClusterRoute(MasterId.fromUserId(user.id))
      }
    }
}
