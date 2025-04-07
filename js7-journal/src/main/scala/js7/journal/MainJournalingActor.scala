package js7.journal

import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import scala.collection.immutable
import scala.concurrent.Future

/**
  * Instances of this trait are thought to be a controlling actor, the only for a journal.
  *
  * @author Joacim Zschimmer
  */
trait MainJournalingActor[S <: JournaledState[S], E <: Event]
extends JournalingActor[S, E]:

  protected final def persist[EE <: E, A](
    keyedEvent: KeyedEvent[EE],
    timestampMillis: Option[Long] = None)
    (callback: (Stamped[KeyedEvent[EE]], S) => A)
  : Future[A] =
    super.persistKeyedEvent(keyedEvent, timestampMillis)(callback)

  protected final def persistMultiple[EE <: E, A](keyedEvents: Iterable[KeyedEvent[EE]])
    (callback: (Seq[Stamped[KeyedEvent[EE]]], S) => A)
  : Future[A] =
    super.persistKeyedEvents(toTimestamped(keyedEvents))(callback)

  protected final def persistTransaction[EE <: E, A](
    keyedEvents: immutable.Iterable[KeyedEvent[EE]])
    (callback: (Seq[Stamped[KeyedEvent[EE]]], S) => A)
  : Future[A] =
    persistTransactionTimestamped(toTimestamped(keyedEvents))(callback)

  protected final def persistTransactionTimestamped[EE <: E, A](keyedEvents: Seq[Timestamped[EE]],
    options: CommitOptions = CommitOptions.default)
    (callback: (Seq[Stamped[KeyedEvent[EE]]], S) => A)
  : Future[A] =
    super.persistKeyedEvents(
      keyedEvents,
      options.copy(transaction = true))(
      callback)
