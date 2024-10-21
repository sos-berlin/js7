package js7.journal.watch

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.Stream
import izumi.reflect.Tag
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.CloseableIterator
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import js7.journal.watch.EventWatch.Every
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.*
import scala.reflect.ClassTag

/**
  * Strict delegator for lazy `CloseableIterator` `EventWatch`.
  * Converts `CloseableIterator` to a `Seq` and closes the iterator.
  *
  * @author Joacim Zschimmer
  */
final class StrictEventWatch(val underlying: FileEventWatch):
  self =>

  private var _lastWatchedEventId = EventId.BeforeFirst

  def lastWatchedEventId: EventId =
    _lastWatchedEventId

  def fileEventIds: Seq[EventId] =
    underlying.fileEventIds

  def stream[E <: Event](
    request: EventRequest[E],
    predicate: KeyedEvent[E] => Boolean = (_: KeyedEvent[E]) => true,
    onlyAcks: Boolean = false)
  : Stream[IO, Stamped[KeyedEvent[E]]] =
    underlying.stream(request, predicate, onlyAcks)

  def when[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean = Every)
  : IO[TearableEventSeq[Seq, KeyedEvent[E]]] =
    delegate(_.when(request, predicate))

  def whenKeyedEvent[E <: Event](using E: Event.KeyCompanion[? >: E])(
    request: EventRequest[E],
    key: E.Key,
    predicate: E => Boolean = Every)
  : IO[E] =
    underlying.whenKeyedEvent(request, key, predicate)

  @TestOnly
  def resetLastWatchedEventId(): Unit =
    _lastWatchedEventId = lastAddedEventId

  /** TEST ONLY - Blocking. */
  @TestOnly
  def awaitNext[E <: Event : ClassTag](
    predicate: KeyedEvent[E] => Boolean = Every,
    timeout: FiniteDuration = 99.s)
    (using IORuntime, Tag[E], sourcecode.Enclosing, sourcecode.FileName, sourcecode.Line)
  : Vector[Stamped[KeyedEvent[E]]] =
    val r = await(predicate, after = _lastWatchedEventId, timeout)
    _lastWatchedEventId = r.last.eventId
    r

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E <: Event: ClassTag](
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId = tornEventId,
    timeout: FiniteDuration = 99.s)
    (using IORuntime, Tag[E],
      sourcecode.Enclosing, sourcecode.FileName, sourcecode.Line)
  : Vector[Stamped[KeyedEvent[E]]] =
    underlying.await(predicate, after, timeout)

  @TestOnly
  def awaitAsync[E <: Event: ClassTag](
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId = EventId.BeforeFirst,
    timeout: FiniteDuration = 99.s)
    (using ioRuntime: IORuntime)
  : IO[Vector[Stamped[KeyedEvent[E]]]] =
    underlying.awaitAsync(predicate, after, timeout)

  @TestOnly
  def awaitKeys[E <: Event : ClassTag : Tag](using E: Event.KeyCompanion[? >: E])
    (keys: IterableOnce[E.Key],
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId = EventId.BeforeFirst,
    timeout: FiniteDuration = 99.s)
    (using IORuntime)
  : Seq[Stamped[KeyedEvent[E]]] =
    underlying
      .untilAllKeys(keys, predicate, after = after, timeout = Some(timeout))
      .logWhenItTakesLonger(s"awaitKeys[${implicitClass[E].shortClassName}]")
      .await(timeout)

  /** TEST ONLY - Blocking. */
  @TestOnly
  def expect[E <: Event](
    predicate: KeyedEvent[E] => Boolean = Every,
    timeout: FiniteDuration = 99.s)
    (using IORuntime, ClassTag[E], Tag[E],
      sourcecode.Enclosing, sourcecode.FileName, sourcecode.Line)
  : Expect[E] =
    val eventId = lastAddedEventId
    Expect(await(predicate, after = eventId, timeout))

  final class Expect[E <: Event : ClassTag] private[StrictEventWatch](await: => Any):
    def apply[A](body: => A): A =
      val a = body
      await
      a

  /** TEST ONLY - Blocking. */
  @TestOnly
  def eventsByKey[E <: Event: ClassTag: Tag](
    using E: Event.KeyCompanion[? >: E], ioRuntime: IORuntime)
    (key: E.Key, after: EventId = tornEventId)
  : Seq[E] =
    keyedEvents[E](after = after).collect:
      case o if o.key == key => o.event

  /** TEST ONLY - Blocking. */
  @TestOnly
  def keyedEvents[E <: Event: ClassTag: Tag](
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId)
    (using ioRuntime: IORuntime)
  : Seq[KeyedEvent[E]] =
    allAfter[E](after = after).await(99.s)
      .filter(stamped => predicate(stamped.value))
      .map(_.value)

  /** TEST ONLY - Blocking. */
  @TestOnly
  def allKeyedEvents[E <: Event: ClassTag](using ioRuntime: IORuntime, E: Tag[E])
  : Seq[KeyedEvent[E]] =
    allStamped[E].map(_.value)

  /** TEST ONLY - Blocking. */
  @TestOnly
  def allStamped[E <: Event: ClassTag](using ioRuntime: IORuntime, E: Tag[E])
  : Seq[Stamped[KeyedEvent[E]]] =
    allAfter[E]().await(99.s)

  @TestOnly
  private def allAfter[E <: Event: ClassTag: Tag](after: EventId = EventId.BeforeFirst)
  : IO[Seq[Stamped[KeyedEvent[E]]]] =
    when[E](EventRequest.singleClass[E](after = after, timeout = Some(0.s)), _ => true)
      .map:
        case TearableEventSeq.Torn(after) =>
          throw new TornException(after = EventId.BeforeFirst, tornEventId = after)
        case EventSeq.Empty(_) => Nil
        case EventSeq.NonEmpty(seq) => seq

  @inline
  private def delegate[A](body: EventWatch => IO[TearableEventSeq[CloseableIterator, A]])
  : IO[TearableEventSeq[Seq, A]] =
    body(underlying).map(_.strict)

  def tornEventId: EventId = underlying.tornEventId

  def lastFileEventId: EventId = underlying.lastFileEventId

  def lastAddedEventId: EventId = underlying.lastAddedEventId
