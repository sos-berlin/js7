package js7.controller.web.serviceprovider

import js7.controller.web.controller.api.SnapshotRoute.SnapshotFilter
import js7.journal.web.GenericEventRoute.StampedEventFilter
import org.apache.pekko.http.scaladsl.server.Route

final case class RouteServiceContext(
  snapshotRoute: SnapshotFilter => Route,
  eventRoute: StampedEventFilter => Route)
