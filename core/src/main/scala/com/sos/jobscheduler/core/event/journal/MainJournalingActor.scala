package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.event.{Event, KeyedEvent, Stamped}
import scala.collection.immutable.Seq
import scala.concurrent.Future

/**
  * Instances of this trait are thought to be a controlling actor, the only for a journal.
  * As a JournalingActor, the actor registers itself at JournalActor.
  * The event `RegisterMe` must arrive before JournalActor takes a snapshot, if `snaphots` is nonEmpty.
  * @author Joacim Zschimmer
  */
trait MainJournalingActor[E <: Event] extends JournalingActor[E] {

  protected final def persistAsync[EE <: E, A](keyedEvent: KeyedEvent[EE], noSync: Boolean = false)(callback: Stamped[KeyedEvent[EE]] ⇒ A): Future[A] =
    super.persistKeyedEvent(keyedEvent, timestamp = None, noSync = noSync, async = true)(callback)

  protected final def persistMultipleAsync[EE <: E, A](keyedEvents: collection.Iterable[KeyedEvent[EE]], noSync: Boolean = false)(callback: Seq[Stamped[KeyedEvent[EE]]] ⇒ A): Future[A] =
    super.persistKeyedEvents(toTimestamped(keyedEvents), noSync = noSync, async = true)(callback)

  protected final def persist[EE <: E, A](keyedEvent: KeyedEvent[EE], timestamp: Option[Timestamp] = None, noSync: Boolean = false, async: Boolean = false)
    (callback: Stamped[KeyedEvent[EE]] ⇒ A)
  : Future[A] =
    super.persistKeyedEvent(keyedEvent, timestamp, noSync = noSync, async = async)(callback)

  protected final def persistMultiple[EE <: E, A](keyedEvents: collection.Iterable[KeyedEvent[EE]], noSync: Boolean = false, async: Boolean = false)
    (callback: Seq[Stamped[KeyedEvent[EE]]] ⇒ A)
  : Future[A] =
    super.persistKeyedEvents(toTimestamped(keyedEvents), noSync = noSync, async = async)(callback)

  protected final def persistTransaction[EE <: E, A](keyedEvents: collection.Iterable[KeyedEvent[EE]], noSync: Boolean = false, async: Boolean = false)
    (callback: Seq[Stamped[KeyedEvent[EE]]] ⇒ A)
  : Future[A] =
    persistTransactionTimestamped(toTimestamped(keyedEvents), noSync = noSync, async = async)(callback)

  protected final def persistTransactionTimestamped[EE <: E, A](keyedEvents: Seq[Timestamped[EE]], noSync: Boolean = false, async: Boolean = false)
    (callback: Seq[Stamped[KeyedEvent[EE]]] ⇒ A)
  : Future[A] =
    super.persistKeyedEvents(keyedEvents, noSync = noSync, async = async, transaction = true)(callback)
}

