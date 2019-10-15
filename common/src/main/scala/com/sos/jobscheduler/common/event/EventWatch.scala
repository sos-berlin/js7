package com.sos.jobscheduler.common.event

import akka.util.ByteString
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

/**
  * @author Joacim Zschimmer
  */
trait EventWatch[E <: Event]
{
  def whenStarted: Future[this.type] = Future.successful(this)

  def strict: StrictEventWatch[E] = new StrictEventWatch(this)

  def observe[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] => Boolean = Every, onlyLastOfChunk: Boolean = false)
  : Observable[Stamped[KeyedEvent[E1]]]

  def read[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E1]]]

  def when[E1 <: E](request: EventRequest[E1], predicate: KeyedEvent[E1] => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E1]]]

  def whenAny[E1 <: E](
    request: EventRequest[E1],
    eventClasses: Set[Class[_ <: E1]],
    predicate: KeyedEvent[E1] => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, KeyedEvent[E1]]]

  def byKey[E1 <: E](
    request: EventRequest[E1],
    key: E1#Key,
    predicate: E1 => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, E1]]

  def whenKeyedEvent[E1 <: E](
    request: EventRequest[E1],
    key: E1#Key,
    predicate: E1 => Boolean = Every)
  : Task[E1]

  def whenKey[E1 <: E](
    request: EventRequest[E1],
    key: E1#Key,
    predicate: E1 => Boolean = Every)
  : Task[TearableEventSeq[CloseableIterator, E1]]

  /** Returns None as last element iff timeout has been elapsed. */
  def observeFile(fileEventId: Option[EventId], position: Option[Long], timeout: FiniteDuration,
    markEOF: Boolean = false, onlyLastOfChunk: Boolean = false)
  : Checked[Observable[PositionAnd[ByteString]]]

  def snapshotObjectsFor(after: EventId): Option[(EventId, CloseableIterator[Any])]

  /** TEST ONLY - Blocking. */
  @TestOnly
  def await[E1 <: E: ClassTag: TypeTag](
    predicate: KeyedEvent[E1] => Boolean = Every,
    after: EventId = EventId.BeforeFirst,
    timeout: FiniteDuration = 99.seconds)
    (implicit s: Scheduler)
  : Vector[Stamped[KeyedEvent[E1]]]

  /** TEST ONLY - Blocking. */
  @TestOnly
  def all[E1 <: E: ClassTag: TypeTag](implicit s: Scheduler): TearableEventSeq[CloseableIterator, KeyedEvent[E1]]

  def tornEventId: EventId

  def lastAddedEventId: EventId
}

object EventWatch
{
  private[event] val Every: Any => Boolean = function1WithToString("Every")(_ => true)
}
