package js7.controller.web.controller.api

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import js7.common.akkahttp.CirceJsonOrYamlSupport._
import js7.controller.web.common.ControllerRouteProvider
import js7.journal.watch.EventWatch

trait JournalInfoRoute extends ControllerRouteProvider
{
  protected def eventWatch: EventWatch

  final lazy val journalInfoRoute: Route =
    pathEnd {
      complete(eventWatch.journalInfo)
    }
}
