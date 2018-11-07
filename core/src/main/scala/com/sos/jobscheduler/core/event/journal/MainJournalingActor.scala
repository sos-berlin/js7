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

  protected final def persistAsync[EE <: E, A](keyedEvent: KeyedEvent[EE])(callback: Stamped[KeyedEvent[EE]] ⇒ A): Future[A] =
    super.persistKeyedEvent(keyedEvent, timestamp = None, async = true)(callback)

  protected final def persistMultipleAsync[EE <: E, A](keyedEvents: collection.Iterable[KeyedEvent[EE]])(callback: Seq[Stamped[KeyedEvent[EE]]] ⇒ A): Future[A] =
    super.persistKeyedEvents(toTimestamped(keyedEvents), async = true)(callback)

  protected final def persist[EE <: E, A](keyedEvent: KeyedEvent[EE], timestamp: Option[Timestamp] = None, async: Boolean = false)
    (callback: Stamped[KeyedEvent[EE]] ⇒ A)
  : Future[A] =
    super.persistKeyedEvent(keyedEvent, timestamp, async = async)(callback)

  protected final def persistMultiple[EE <: E, A](keyedEvents: collection.Iterable[KeyedEvent[EE]], async: Boolean = false)
    (callback: Seq[Stamped[KeyedEvent[EE]]] ⇒ A)
  : Future[A] =
    super.persistKeyedEvents(toTimestamped(keyedEvents), async = async)(callback)

  protected final def persistTransaction[EE <: E, A](keyedEvents: collection.Iterable[KeyedEvent[EE]], async: Boolean = false)
    (callback: Seq[Stamped[KeyedEvent[EE]]] ⇒ A)
  : Future[A] =
    persistTransactionTimestamped(toTimestamped(keyedEvents), async = async)(callback)

  protected final def persistTransactionTimestamped[EE <: E, A](keyedEvents: Seq[Timestamped[EE]], async: Boolean = false)
    (callback: Seq[Stamped[KeyedEvent[EE]]] ⇒ A)
  : Future[A] =
    super.persistKeyedEvents(keyedEvents, async = async, transaction = true)(callback)
}

