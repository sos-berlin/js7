package js7.agent.web.controller

import js7.common.akkahttp.AkkaHttpServerUtils.pathSegment
import js7.core.cluster.ClusterWatchRoute

trait ControllerRoute extends ControllersEventRoute with ClusterWatchRoute
{
  protected final lazy val controllerRoute =
    pathSegment("event") {
      controllerEventRoute
    }
}
