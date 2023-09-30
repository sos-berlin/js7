package js7.agent.web

import akka.http.scaladsl.model.StatusCodes.NotFound
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import js7.agent.web.views.RootWebService
import js7.cluster.web.ClusterRoute
import js7.common.akkahttp.web.session.SessionRoute
import js7.journal.web.JournalRoute

/**
  * @author Joacim Zschimmer
  */
trait ApiRoute
extends RootWebService
with CommandWebService
with EventRoute
with SessionRoute
with JournalRoute
with ClusterRoute:

  protected final val apiRoute: Route =
    pathPrefix(Segment) {
      case "event" => eventRoute
      case "command" => commandRoute
      case "session" => sessionRoute
      case "journal" => journalRoute
      case "cluster" => clusterRoute
      case _ => complete(NotFound)
    } ~
    pathEndOrSingleSlash:
      apiRootRoute
