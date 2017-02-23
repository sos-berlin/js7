package com.sos.scheduler.engine.shared.event.journal

import com.sos.scheduler.engine.data.event.{Event, KeyedEvent, Snapshot}

/**
  * @author Joacim Zschimmer
  */
trait KeyedEventJournalingActor[E <: Event] extends JournalingActor[E] {

  override def preStart() = {
    journalActor ! Journal.Input.RegisterMe(None)
    super.preStart()
  }

  protected final def persist[EE <: E](keyedEvent: KeyedEvent[EE])(callback: Snapshot[KeyedEvent[EE]] ⇒ Unit): Unit =
    super.persistKeyedEvent(keyedEvent)(callback)
}
