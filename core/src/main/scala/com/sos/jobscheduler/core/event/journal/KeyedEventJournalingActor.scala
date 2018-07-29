package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.event.{Event, KeyedEvent, Stamped}
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
trait KeyedEventJournalingActor[E <: Event] extends JournalingActor[E] {

  override def preStart() = {
    journalActor ! JournalActor.Input.RegisterMe(None)
    super.preStart()
  }

  protected final def persistAsync[EE <: E, A](keyedEvent: KeyedEvent[EE], noSync: Boolean = false)(callback: Stamped[KeyedEvent[EE]] ⇒ A): Future[A] =
    persist(keyedEvent, async = true)(callback)

  protected final def persist[EE <: E, A](keyedEvent: KeyedEvent[EE], timestamp: Option[Timestamp] = None, async: Boolean = false, noSync: Boolean = false)
    (callback: Stamped[KeyedEvent[EE]] ⇒ A)
  : Future[A] =
    super.persistKeyedEvent(keyedEvent, timestamp, noSync = noSync, async = async)(callback)
}
