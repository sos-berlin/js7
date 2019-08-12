package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.problem.Checked
import monix.reactive.Observable

trait JournaledState[Self <: JournaledState[Self, E], E <: Event]
{
  def applyEvent: PartialFunction[KeyedEvent[E], Checked[Self]]

  def withEventId(eventId: EventId): Self

  def toSnapshotObservable: Observable[Any]
}
