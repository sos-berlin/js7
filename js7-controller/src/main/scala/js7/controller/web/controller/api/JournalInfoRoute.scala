package js7.controller.web.controller.api

import js7.common.pekkohttp.CirceJsonSupport.*
import js7.controller.web.common.ControllerRouteProvider
import js7.journal.watch.EventWatch
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route

trait JournalInfoRoute extends ControllerRouteProvider:
  protected def eventWatch: EventWatch

  final lazy val journalInfoRoute: Route =
    pathEnd:
      complete(eventWatch.journalInfo)
