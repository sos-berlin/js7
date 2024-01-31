package js7.proxy.data.event

import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}

final case class EventAndState[+E <: Event, S <: JournaledState[S]](
  stampedEvent: Stamped[KeyedEvent[E]],
  previousState: S,
  state: S)
