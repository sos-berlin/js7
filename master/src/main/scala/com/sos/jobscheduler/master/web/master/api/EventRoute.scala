package com.sos.jobscheduler.master.web.master.api

import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.common.event.EventWatch
import com.sos.jobscheduler.core.event.AbstractEventRoute
import com.sos.jobscheduler.data.event.{Event, KeyedEvent}
import com.sos.jobscheduler.master.agent.AgentEventIdEvent
import com.sos.jobscheduler.master.configuration.KeyedEventJsonCodecs.MasterKeyedEventJsonCodec
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait EventRoute extends MasterRouteProvider with AbstractEventRoute[Event]
{
  protected def eventWatch: EventWatch[Event]

  protected final def eventClass =
    classOf[Event]

  protected final def keyedEventTypedJsonCodec =
    MasterKeyedEventJsonCodec

  protected final def eventWatchFor(userId: UserId) =
    Task.pure(eventWatch)

  override protected final def isRelevantEvent(keyedEvent: KeyedEvent[Event]) =
    EventRoute.isRelevantEvent(keyedEvent)

  protected final def eventRoute = abstractEventRoute
}

object EventRoute
{
  private def isRelevantEvent(keyedEvent: KeyedEvent[_ <: Event]): Boolean =
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
