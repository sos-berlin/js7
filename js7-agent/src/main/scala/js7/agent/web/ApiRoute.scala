package js7.agent.web

import js7.agent.web.views.RootWebService
import js7.base.auth.ValidUserPermission
import js7.base.problem.Problem
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkohttp.web.session.SessionRoute
import js7.core.cluster.watch.ClusterWatchRoute
import js7.data.controller.ControllerId
import org.apache.pekko.http.scaladsl.model.StatusCodes.NotFound
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route

/**
  * @author Joacim Zschimmer
  */
trait ApiRoute
extends RootWebService
with CommandWebService
with EventRoute
with SessionRoute
with ClusterWatchRoute
{
  protected final val apiRoute: Route =
    pathPrefix(Segment) {
      case "event" => eventRoute
      case "command" => commandRoute
      case "clusterWatch" => clusterWatchRoute
      case "session" => sessionRoute
      case _ => complete(NotFound)
    } ~
    pathEndOrSingleSlash {
      apiRootRoute
    }

  protected val clusterWatchRoute =
    pathEnd {
      maybeSession(Set(ValidUserPermission)) {
        case (_, None) =>
          complete(Problem("ClusterWatch requires a login session")) // ???
        case (user, Some(session)) =>
          clusterWatchRouteFor(ControllerId.fromUserId(user.id), session)
      }
    }
}
