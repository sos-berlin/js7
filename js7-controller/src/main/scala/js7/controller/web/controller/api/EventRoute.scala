package js7.controller.web.controller.api

import js7.base.auth.SimpleUser
import js7.controller.data.ControllerState
import js7.controller.web.common.ControllerRouteProvider
import js7.journal.watch.EventWatch
import js7.journal.web.GenericEventRoute
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait EventRoute extends ControllerRouteProvider with GenericEventRoute
{
  protected def eventWatch: EventWatch

  protected final lazy val eventRoute = new GenericEventRouteProvider {
    def keyedEventTypedJsonCodec = ControllerState.keyedEventJsonCodec
    def eventWatchFor(user: SimpleUser) = Task.pure(Right(eventWatch))
  }.route
}
