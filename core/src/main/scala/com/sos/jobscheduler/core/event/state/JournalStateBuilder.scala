package com.sos.jobscheduler.core.event.state

import com.sos.jobscheduler.data.event.{Event, EventId, JournaledState, KeyedEvent, Stamped}

trait JournalStateBuilder[S <: JournaledState[S, E], E <: Event]
{
  def addSnapshot: PartialFunction[Any, Unit]

  def onAllSnapshotsAdded(): Unit

  def addEvent: PartialFunction[Stamped[KeyedEvent[Event]], Unit]

  def state(eventId: EventId): S
}
