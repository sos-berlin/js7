package com.sos.jobscheduler.master.web.master.api

import com.sos.jobscheduler.base.auth.SimpleUser
import com.sos.jobscheduler.common.event.EventWatch
import com.sos.jobscheduler.core.event.GenericEventRoute
import com.sos.jobscheduler.data.cluster.ClusterEvent
import com.sos.jobscheduler.data.event.{Event, KeyedEvent}
import com.sos.jobscheduler.data.filebased.RepoEvent
import com.sos.jobscheduler.data.order.OrderEvent
import com.sos.jobscheduler.master.data.events.{MasterAgentEvent, MasterEvent, MasterKeyedEventJsonCodec}
import com.sos.jobscheduler.master.web.common.MasterRouteProvider
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait EventRoute extends MasterRouteProvider with GenericEventRoute
{
  protected def eventWatch: EventWatch[Event]

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
