package js7.controller.web.controller.api

import js7.controller.web.common.ControllerRouteProvider
import js7.data.controller.ControllerState
import js7.journal.watch.EventWatch
import js7.journal.web.GenericEventRoute
import js7.journal.web.GenericEventRoute.StampedEventFilter
import org.apache.pekko.http.scaladsl.server.Route

/**
  * @author Joacim Zschimmer
  */
trait EventRoute extends ControllerRouteProvider with GenericEventRoute
{
  protected def eventWatch: EventWatch

  protected lazy val eventRoute = filteredEventRoute(identity)

  protected final def filteredEventRoute(filter: StampedEventFilter): Route =
    new GenericEventRouteProvider {
      def keyedEventTypedJsonCodec = ControllerState.keyedEventJsonCodec
      override def filterObservable = filter
    }.route
}
