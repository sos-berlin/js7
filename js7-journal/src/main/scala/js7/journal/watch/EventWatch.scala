package js7.journal.watch

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.Stream
import izumi.reflect.Tag
import js7.base.catsutils.CatsEffectExtensions.fromFutureDummyCancelable
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.problem.Checked
import js7.base.time.ScalaTime.*
import js7.base.utils.CloseableIterator
import js7.base.utils.ScalaUtils.function1WithToString
import js7.data.event.{Event, EventId, EventRequest, JournalInfo, KeyedEvent, Stamped, TearableEventSeq}
import js7.journal.watch.EventWatch.*
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait EventWatch:

  val started: IO[this.type] =
    memoize:
      IO.fromFutureDummyCancelable(IO(whenStarted)).asInstanceOf[IO[this.type]]

  def whenStarted: Future[this.type] = Future.successful(this)

  def stream[E <: Event](
    request: EventRequest[E],
    predicate: KeyedEvent[E] => Boolean = Every,
    onlyAcks: Boolean = false)
  : Stream[IO, Stamped[KeyedEvent[E]]]

  def streamEventIds(timeout: Option[FiniteDuration]): IO[Checked[Stream[IO, EventId]]]

  def when[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean = Every)
  : IO[TearableEventSeq[CloseableIterator, KeyedEvent[E]]]

  def whenKeyedEvent[E <: Event](using E: Event.KeyCompanion[? >: E])(
    request: EventRequest[E],
    key: E.Key,
    predicate: E => Boolean = Every)
  : IO[E]

  def whenKey[E <: Event](using E: Event.KeyCompanion[? >: E])
    (request: EventRequest[E],
    key: E.Key,
    predicate: E => Boolean = Every)
  : IO[TearableEventSeq[CloseableIterator, E]]

  def journalInfo: JournalInfo

  @TestOnly
  def untilAllKeys[E <: Event : ClassTag](using E: Event.KeyCompanion[? >: E])
    (keys: IterableOnce[E.Key],
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId,
    timeout: Option[FiniteDuration])
  : IO[Seq[Stamped[KeyedEvent[E]]]] =
    stream(EventRequest.singleClass[E](after = after, timeout = timeout))
      .filter(stamped => predicate(stamped.value))
      .mapAccumulate(Set.from(keys)): (keys, stamped) =>
        val minishedKeys = keys - stamped.value.key.asInstanceOf[E.Key]
        (minishedKeys, (stamped, minishedKeys.nonEmpty))
      .map(_._2)
      .takeWhile(_._2)
      .map(_._1)
      .compile
      .toList

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E <: Event: ClassTag: Tag](
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId = EventId.BeforeFirst,
    timeout: FiniteDuration = 99.s)
    (using IORuntime, sourcecode.Enclosing, sourcecode.FileName, sourcecode.Line)
  : Vector[Stamped[KeyedEvent[E]]]

  @TestOnly
  def awaitAsync[E <: Event](
    eventRequest: EventRequest[E],
    predicate: KeyedEvent[E] => Boolean)
    (using IORuntime, sourcecode.Enclosing, sourcecode.FileName, sourcecode.Line)
  : IO[Vector[Stamped[KeyedEvent[E]]]]

  @TestOnly
  def awaitAsync[E <: Event: ClassTag](
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId = EventId.BeforeFirst,
    timeout: FiniteDuration = 99.s)
    (using IORuntime, sourcecode.Enclosing, sourcecode.FileName, sourcecode.Line)
  : IO[Vector[Stamped[KeyedEvent[E]]]]

  def lastAddedEventId: EventId

  def tornEventId: EventId

  def checkEventId(eventId: EventId, tornOlder: Option[FiniteDuration] = None): Checked[Unit]


object EventWatch:
  private[watch] val Every: Any => Boolean = function1WithToString("Every")(_ => true)
