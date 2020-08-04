package js7.controller.web.controller.api

import js7.base.auth.SimpleUser
import js7.common.event.EventWatch
import js7.controller.data.events.{ControllerAgentEvent, ControllerEvent, ControllerKeyedEventJsonCodec}
import js7.controller.web.common.ControllerRouteProvider
import js7.core.event.GenericEventRoute
import js7.data.cluster.ClusterEvent
import js7.data.event.{Event, KeyedEvent}
import js7.data.item.RepoEvent
import js7.data.order.OrderEvent
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait EventRoute extends ControllerRouteProvider with GenericEventRoute
{
  protected def eventWatch: EventWatch

  protected final lazy val eventRoute = new GenericEventRouteProvider {
    def keyedEventTypedJsonCodec = ControllerKeyedEventJsonCodec
    def eventWatchFor(user: SimpleUser) = Task.pure(Right(eventWatch))
    override def isRelevantEvent(keyedEvent: KeyedEvent[Event]) = EventRoute.isRelevantEvent(keyedEvent)
  }.route
}

object EventRoute
{
  private def isRelevantEvent(keyedEvent: KeyedEvent[Event]): Boolean =
    isRelevantEvent(keyedEvent.event)

  private def isRelevantEvent(event: Event): Boolean =
    event match {
      case _: RepoEvent | _: ControllerEvent | _: ControllerAgentEvent | _: OrderEvent | _: ClusterEvent => true
      case _ => false
    }
}
