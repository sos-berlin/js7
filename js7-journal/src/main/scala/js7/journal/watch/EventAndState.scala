package js7.journal.watch

import js7.data.event.{Event, JournaledState, KeyedEvent}

final case class EventAndState[+E <: Event, S <: JournaledState[S]](
  keyedEvent: KeyedEvent[E],
  state: S)
