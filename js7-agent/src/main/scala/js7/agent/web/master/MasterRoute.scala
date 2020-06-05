package js7.agent.web.master

import akka.http.scaladsl.server.RouteConcatenation._
import js7.base.auth.ValidUserPermission
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.core.cluster.MastersClusterRoute
import js7.data.master.MasterId

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
