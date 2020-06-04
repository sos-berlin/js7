package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.utils.ScalaUtils.function1WithToString
import com.sos.jobscheduler.common.event.EventWatch.Every
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, KeyedEvent, Stamped, TearableEventSeq}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scodec.bits.ByteVector

/**
  * @author Joacim Zschimmer
  */
trait EventWatch
{
  def started: Task[this.type] =
    Task.fromFuture(whenStarted).memoize.asInstanceOf[Task[this.type]]

  def whenStarted: Future[this.type] = Future.successful(this)

  def strict: StrictEventWatch = new StrictEventWatch(this)

  def observe[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean = Every, onlyLastOfChunk: Boolean = false)
  : Observable[Stamped[KeyedEvent[E]]]

  def read[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E]]]

  def when[E <: Event](request: EventRequest[E], predicate: KeyedEvent[E] => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E]]]

  def whenAny[E <: Event](
    request: EventRequest[E],
    eventClasses: Set[Class[_ <: E]],
    predicate: KeyedEvent[E] => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E]]]

  def byKey[E <: Event](
    request: EventRequest[E],
    key: E#Key,
    predicate: E => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, E]]

  def whenKeyedEvent[E <: Event](
    request: EventRequest[E],
    key: E#Key,
    predicate: E => Boolean = Every)
  : Task[E]

  def whenKey[E <: Event](
    request: EventRequest[E],
    key: E#Key,
    predicate: E => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, E]]

  /** Returns None as last element iff timeout has been elapsed. */
  def observeFile(fileEventId: Option[EventId], position: Option[Long], timeout: FiniteDuration,
    markEOF: Boolean = false, onlyLastOfChunk: Boolean = false)
  : Checked[Observable[PositionAnd[ByteVector]]]

  def snapshotObjectsFor(after: EventId): Option[(EventId, CloseableIterator[Any])]

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E <: Event: ClassTag: TypeTag](
    predicate: KeyedEvent[E] => Boolean = Every,
    after: EventId = EventId.BeforeFirst,
    timeout: FiniteDuration = 99.seconds)
    (implicit s: Scheduler)
  : Vector[Stamped[KeyedEvent[E]]]

  /** TEST ONLY - Blocking. */
  @TestOnly
  def all[E <: Event: ClassTag: TypeTag](implicit s: Scheduler): TearableEventSeq[CloseableIterator, KeyedEvent[E]]

  def fileEventIds: Seq[EventId]

  final def tornEventId = fileEventIds.headOption getOrElse EventId.BeforeFirst

  final def lastFileTornEventId = fileEventIds.last

  def lastAddedEventId: EventId
}

object EventWatch
{
  private[event] val Every: Any => Boolean = function1WithToString("Every")(_ => true)
}
