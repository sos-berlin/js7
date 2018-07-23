package com.sos.jobscheduler.master.web.master.api

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.common.event.EventWatch
import com.sos.jobscheduler.core.event.GenericEventRoute
import com.sos.jobscheduler.data.event.{Event, KeyedEvent}
import com.sos.jobscheduler.master.agent.AgentEventIdEvent
import com.sos.jobscheduler.master.configuration.KeyedEventJsonCodecs.MasterKeyedEventJsonCodec
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait EventRoute extends MasterRouteProvider with GenericEventRoute
{
  protected def eventWatch: EventWatch[Event]

  protected final lazy val eventRoute = new RouteProvider().route

  private class RouteProvider extends GenericEventRouteProvider
  {
    def keyedEventTypedJsonCodec = MasterKeyedEventJsonCodec

    def eventWatchFor(userId: UserId) = Task.pure(eventWatch)

    override def isRelevantEvent(keyedEvent: KeyedEvent[Event]) = EventRoute.isRelevantEvent(keyedEvent)
  }
}

object EventRoute
{
  private def isRelevantEvent(keyedEvent: KeyedEvent[Event]): Boolean =
    isRelevantEvent(keyedEvent.event)

  private def isRelevantEvent(event: Event): Boolean =
    event match {
      case //_: OrderEvent.OrderDetachable |
        _: AgentEventIdEvent ⇒
        false
      case _ ⇒
        true
    }
}
