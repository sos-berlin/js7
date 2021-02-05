package js7.controller.web.controller.api

import akka.http.scaladsl.server.Route
import js7.base.auth.SimpleUser
import js7.controller.web.common.ControllerRouteProvider
import js7.data.controller.ControllerState
import js7.journal.watch.EventWatch
import js7.journal.web.GenericEventRoute
import js7.journal.web.GenericEventRoute.StampedEventFilter
import monix.eval.Task

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
      def eventWatchFor(user: SimpleUser) = Task.pure(Right(eventWatch))
      override def filterObservable = filter
    }.route
}
