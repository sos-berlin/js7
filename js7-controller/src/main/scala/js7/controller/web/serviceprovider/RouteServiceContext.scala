package js7.controller.web.serviceprovider

import org.apache.pekko.http.scaladsl.server.Route
import com.typesafe.config.Config
import js7.controller.web.controller.api.SnapshotRoute.SnapshotFilter
import js7.journal.web.GenericEventRoute.StampedEventFilter

final case class RouteServiceContext(
  snapshotRoute: SnapshotFilter => Route,
  eventRoute: StampedEventFilter => Route,
  config: Config)
