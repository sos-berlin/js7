package com.sos.jobscheduler.shared.event.journal

import com.sos.jobscheduler.data.event.{Event, KeyedEvent, Stamped}

/**
  * @author Joacim Zschimmer
  */
trait KeyedEventJournalingActor[E <: Event] extends JournalingActor[E] {

  override def preStart() = {
    journalActor ! Journal.Input.RegisterMe(None)
    super.preStart()
  }

  protected final def persist[EE <: E](keyedEvent: KeyedEvent[EE])(callback: Stamped[KeyedEvent[EE]] ⇒ Unit): Unit =
    super.persistKeyedEvent(keyedEvent)(callback)

  protected final def persistAsync[EE <: E](keyedEvent: KeyedEvent[EE])(callback: Stamped[KeyedEvent[EE]] ⇒ Unit): Unit =
    super.persistAsyncKeyedEvent(keyedEvent)(callback)
}
