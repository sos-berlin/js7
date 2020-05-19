package com.sos.jobscheduler.proxy

import com.sos.jobscheduler.data.event.{Event, JournaledState, KeyedEvent, Stamped}

final case class EventAndState[+E <: Event, S <: JournaledState[S]](
  stampedEvent: Stamped[KeyedEvent[E]],
  state: S)
