package js7.master.web.master.api

import js7.base.auth.SimpleUser
import js7.common.event.EventWatch
import js7.core.event.GenericEventRoute
import js7.data.cluster.ClusterEvent
import js7.data.event.{Event, KeyedEvent}
import js7.data.filebased.RepoEvent
import js7.data.order.OrderEvent
import js7.master.data.events.{MasterAgentEvent, MasterEvent, MasterKeyedEventJsonCodec}
import js7.master.web.common.MasterRouteProvider
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait EventRoute extends MasterRouteProvider with GenericEventRoute
{
  protected def eventWatch: EventWatch

  protected final lazy val eventRoute = new GenericEventRouteProvider {
    def keyedEventTypedJsonCodec = MasterKeyedEventJsonCodec
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
      case _: RepoEvent | _: MasterEvent | _: MasterAgentEvent | _: OrderEvent | _: ClusterEvent => true
      case _ => false
    }
}
