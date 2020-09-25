package js7.core.event.journal

import js7.base.time.Timestamp
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * Instances of this trait are thought to be a controlling actor, the only for a journal.
  * @author Joacim Zschimmer
  */
trait MainJournalingActor[S <: JournaledState[S], E <: Event]
extends JournalingActor[S, E]
{
  protected final def persistAsync[EE <: E, A](keyedEvent: KeyedEvent[EE])(callback: (Stamped[KeyedEvent[EE]], S) => A): Future[A] =
    super.persistKeyedEvent(keyedEvent, timestamp = None, async = true)(callback)

  protected final def persistMultipleAsync[EE <: E, A](keyedEvents: collection.Iterable[KeyedEvent[EE]])
    (callback: (Seq[Stamped[KeyedEvent[EE]]], S) => A)
  : Future[A] =
    super.persistKeyedEvents(toTimestamped(keyedEvents), async = true)(callback)

  protected final def persist[EE <: E, A](keyedEvent: KeyedEvent[EE], timestamp: Option[Timestamp] = None, async: Boolean = false)
    (callback: (Stamped[KeyedEvent[EE]], S) => A)
  : Future[A] =
    super.persistKeyedEvent(keyedEvent, timestamp, async = async)(callback)

  protected final def persistMultiple[EE <: E, A](keyedEvents: collection.Iterable[KeyedEvent[EE]], async: Boolean = false)
    (callback: (Seq[Stamped[KeyedEvent[EE]]], S) => A)
  : Future[A] =
    super.persistKeyedEvents(toTimestamped(keyedEvents), async = async)(callback)

  protected final def persistTransaction[EE <: E, A](keyedEvents: collection.Iterable[KeyedEvent[EE]], async: Boolean = false)
    (callback: (Seq[Stamped[KeyedEvent[EE]]], S) => A)
  : Future[A] =
    persistTransactionTimestamped(toTimestamped(keyedEvents), async = async)(callback)

  protected final def persistTransactionTimestamped[EE <: E, A](keyedEvents: Seq[Timestamped[EE]],
    async: Boolean = false, delay: FiniteDuration = Duration.Zero, alreadyDelayed: FiniteDuration = Duration.Zero)
    (callback: (Seq[Stamped[KeyedEvent[EE]]], S) => A)
  : Future[A] =
    super.persistKeyedEvents(keyedEvents, async = async, transaction = true, delay = delay, alreadyDelayed = alreadyDelayed)(callback)
}

