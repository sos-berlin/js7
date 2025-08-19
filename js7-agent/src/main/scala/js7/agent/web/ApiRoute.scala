package js7.agent.web

import js7.agent.web.views.RootWebService
import js7.cluster.web.ClusterRoute
import js7.common.pekkohttp.web.session.SessionRoute
import js7.journal.web.JournalRoute
import org.apache.pekko.http.scaladsl.model.StatusCodes.NotFound
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route

/**
  * @author Joacim Zschimmer
  */
trait ApiRoute
extends
  RootWebService,
  CommandWebService,
  EventRoute,
  SessionRoute,
  JournalRoute,
  ClusterRoute:

  protected final val apiRoute: Route =
    pathPrefix(Segment):
      case "event" => eventRoute
      case "command" => commandRoute
      case "session" => sessionRoute
      case "journal" => journalRoute
      case "cluster" => clusterRoute
      case _ => complete(NotFound)
    ~
      pathEndOrSingleSlash:
        apiRootRoute
