package js7.data.event

import cats.Monoid
import js7.base.problem.{Checked, Problem}
import js7.base.time.{Timestamp, WallClock}
import js7.base.utils.ScalaUtils.syntax.toEagerSeq
import js7.data.event.KeyedEvent.NoKey
import org.jetbrains.annotations.TestOnly

/** Calculator for EventColl.
  *
  * In essence, a function `EventColl => Checked[EventColl]`
  */
final class EventCalc[S <: EventDrivenState[S, E], E <: Event, Ctx](
  private val function: EventColl[S, E, Ctx] => Checked[EventColl[S, E, Ctx]]):

  def combine[S1 >: S <: EventDrivenState[S1, E1], E1 >: E <: Event, Ctx1](
    other: EventCalc[S1, E1, Ctx & Ctx1])
  : EventCalc[S1, E1, Ctx & Ctx1] =
    EventCalc[S1, E1, Ctx & Ctx1]: coll =>
      widen[S1, E1, Ctx & Ctx1].calculate(coll).flatMap(other.calculate)

  @TestOnly // ???
  def calculateEventsAndAggregate(aggregate: S, context: Ctx): Checked[(Vector[KeyedEvent[E]], S)] =
    calculate(aggregate, context).map: coll =>
      coll.keyedEvents -> coll.aggregate

  /** Returns an EventColl containing the calculated events and aggregate. */
  def calculate(aggregate: S, context: Ctx): Checked[EventColl[S, E, Ctx]] =
    calculate[S, E, Ctx](EventColl(aggregate, context))

  /** Returns an EventColl containing the calculated events and aggregate. */
  inline def calculate[S1 >: S <: EventDrivenState[S1, E1], E1 >: E <: Event, Ctx1](
    eventColl: EventColl[S1, E1, Ctx & Ctx1])
  : Checked[EventColl[S1, E1, Ctx & Ctx1]] =
    widen[S1, E1, Ctx & Ctx1].function(eventColl)

  inline def widen[S1 >: S <: EventDrivenState[S1, E1], E1 >: E <: Event, Ctx1]
  : EventCalc[S1, E1, Ctx & Ctx1] =
    this.asInstanceOf[EventCalc[S1, E1, Ctx & Ctx1]]


object EventCalc:

  private val Empty: EventCalc[?, ?, ?] =
    EventCalc(Right(_))

  inline def empty[S <: EventDrivenState[S, E], E <: Event, Ctx]: EventCalc[S, E, Ctx] =
    Empty.asInstanceOf

  inline def apply[S <: EventDrivenState[S, E], E <: Event, Ctx](
    function: EventColl[S, E, Ctx] => Checked[EventColl[S, E, Ctx]])
  : EventCalc[S, E, Ctx] =
    new EventCalc(function)

  def problem[S <: EventDrivenState[S, E], E <: Event, Ctx](problem: Problem): EventCalc[S, E, Ctx] =
    EventCalc(_ => Left(problem))

  def single[S <: EventDrivenState[S, E], E <: Event, Ctx](
    toKeyedEvent: S => OpaqueEventColl[S, E, Ctx] ?=> KeyedEvent[E])
  : EventCalc[S, E, Ctx] =
    EventCalc: coll =>
      coll.addEvent(toKeyedEvent(coll.aggregate)(using coll))

  inline def maybe[S <: EventDrivenState[S, E], E <: Event, Ctx](
    toKeyedEvents: S => OpaqueEventColl[S, E, Ctx] ?=> Option[MaybeTimestampedKeyedEvent[E]])
  : EventCalc[S, E, Ctx] =
    multiple(toKeyedEvents)

  def multiple[S <: EventDrivenState[S, E], E <: Event, Ctx](
    toKeyedEvents: S => OpaqueEventColl[S, E, Ctx] ?=> IterableOnce[MaybeTimestampedKeyedEvent[E]])
  : EventCalc[S, E, Ctx] =
    EventCalc: coll =>
      coll.addEvents(toKeyedEvents(coll.aggregate)(using coll))

  def checked[S <: EventDrivenState[S, E], E <: Event, Ctx](
    toCheckedKeyedEvents: S => OpaqueEventColl[S, E, Ctx] ?=> Checked[IterableOnce[KeyedEvent[E]]])
  : EventCalc[S, E, Ctx] =
    EventCalc: coll =>
      coll.addChecked(toCheckedKeyedEvents(coll.aggregate)(using coll))

  def addChecked[S <: EventDrivenState[S, E], E <: Event, Ctx](
    keyedEvents: Checked[IterableOnce[KeyedEvent[E]]])
  : EventCalc[S, E, Ctx] =
    val eagerlyComputedKeyedEvents = keyedEvents.map(_.toEagerSeq)
    EventCalc(_.addChecked(eagerlyComputedKeyedEvents))

  inline def pure[S <: EventDrivenState[S, E], E <: Event, Ctx](
    keyedEvent: MaybeTimestampedKeyedEvent[E])
  : EventCalc[S, E, Ctx] =
    pureEvent(keyedEvent)

  inline def pure[S <: EventDrivenState[S, E], E <: Event, Ctx](
    keyedEvents: MaybeTimestampedKeyedEvent[E]*)
  : EventCalc[S, E, Ctx] =
    pureEvents(keyedEvents)

  inline def pure[S <: EventDrivenState[S, E], E <: Event, Ctx](
    keyedEvents: IterableOnce[MaybeTimestampedKeyedEvent[E]])
  : EventCalc[S, E, Ctx] =
    pureEvents(keyedEvents)

  //inline def pure[K, E1 <: E](key: K)(events: Iterable[E1])
  //  (using /*erased*/ E1: Event.KeyCompanion[? >: E1])
  //  (using /*erased*/ ev: K =:= E1.Key)
  //: Checked[EventCalc[S, E, Ctx] =
  //  pureWithKey[K, E1](key)(events)

  def pureNoKey[S <: EventDrivenState[S, E], E <: NoKeyEvent, Ctx](events: Iterable[E])
  : EventCalc[S, E, Ctx] =
    val eagerlyComputedKeyedEvents = events.view.map(NoKey <-: _).toVector
    EventCalc(_.add(eagerlyComputedKeyedEvents))

  def pureEvent[S <: EventDrivenState[S, E], E <: Event, Ctx](
    keyedEvent: MaybeTimestampedKeyedEvent[E])
  : EventCalc[S, E, Ctx] =
    EventCalc(_.addEvent(keyedEvent))

  private def pureEvents[S <: EventDrivenState[S, E], E <: Event, Ctx](
    keyedEvents: IterableOnce[MaybeTimestampedKeyedEvent[E]])
  : EventCalc[S, E, Ctx] =
    val eagerlyComputedKeyedEvents = keyedEvents.toEagerSeq
    EventCalc(_.addEvents(keyedEvents))

  //private val Monoid = new Monoid[EventCalc[?, ?, ?]]:
  //  def empty = EventCalc.empty
  //
  //  def combine(a: EventCalc[?, ?, ?], b: EventCalc[?, ?, ?]): EventCalc[?, ?, ?] =
  //    a.combine[?, ?, ?](b)

  opaque type OpaqueEventColl[S <: EventDrivenState[S, E], E <: Event, Ctx] = EventColl[S, E, Ctx]

  def context[S <: EventDrivenState[S, E], E <: Event, Ctx](using coll: OpaqueEventColl[S, E, Ctx])
  : Ctx =
    coll.context

  /** TODO Don't use this, use `now`. */
  inline def clock[S <: EventDrivenState[S, E], E <: Event](
    using coll: OpaqueEventColl[S, E, TimeCtx])
  : WallClock =
    context.clock

  def now[S <: EventDrivenState[S, E], E <: Event]()(
    using coll: OpaqueEventColl[S, E, TimeCtx])
  : Timestamp =
    coll.context.now

  // Monoid //
  given [S <: EventDrivenState[S, E], E <: Event, Ctx] => Monoid[EventCalc[S, E, Ctx]] =
    type EC = EventCalc[S, E, Ctx]
    new Monoid[EC]:
      def empty = EventCalc.empty

      def combine(a: EC, b: EC): EC =
        a.combine(b)

  inline def combine[S <: EventDrivenState[S, E], E <: Event, Ctx](
    eventCalcs: EventCalc[S, E, Ctx]*)
  : EventCalc[S, E, Ctx] =
    combineAll(eventCalcs)

  def combineAll[S <: EventDrivenState[S, E], E <: Event, Ctx](
    eventCalcs: Iterable[EventCalc[S, E, Ctx]])
  : EventCalc[S, E, Ctx] =
    eventCalcs.view.scan(empty)(_.combine(_)).last
