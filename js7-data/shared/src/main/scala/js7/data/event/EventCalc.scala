package js7.data.event

import cats.Monoid
import js7.base.problem.{Checked, Problem}
import js7.base.time.{Timestamp, WallClock}
import js7.base.utils.ScalaUtils.syntax.toEagerSeq
import js7.data.event
import js7.data.event.EventCalcCtx.OpaqueEventCollCtx
import js7.data.event.KeyedEvent.NoKey
import org.jetbrains.annotations.TestOnly
import scala.annotation.tailrec

/** Calculator for EventColl.
  *
  * In essence, a function `EventCollCtx => Checked[EventColl]`
  */
final class EventCalcCtx[S <: EventDrivenState_[S, E], E <: Event, Ctx](
  private val function: EventCollCtx[S, E, Ctx] => Checked[EventCollCtx[S, E, Ctx]]):

  @TestOnly
  def calculateEventsAndAggregate(aggregate: S, context: Ctx): Checked[(Vector[KeyedEvent[E]], S)] =
    calculate(aggregate, context).map: coll =>
      coll.keyedEvents.toVector -> coll.aggregate

  /** Returns an EventCollCtx containing the calculated events and aggregate. */
  def calculate(aggregate: S, context: Ctx): Checked[EventCollCtx[S, E, Ctx]] =
    calculate[S, E, Ctx](EventCollCtx(aggregate, context))

  /** Returns an EventCollCtx containing the calculated events and aggregate.
    * <p>Simply calls  `function`. */
  inline def calculate[S1 >: S <: EventDrivenState_[S1, E1], E1 >: E <: Event, Ctx1](
    eventColl: EventCollCtx[S1, E1, Ctx & Ctx1])
  : Checked[EventCollCtx[S1, E1, Ctx & Ctx1]] =
    widen[S1, E1, Ctx & Ctx1].function(eventColl)

  inline def widen[S1 >: S <: EventDrivenState_[S1, E1], E1 >: E <: Event, Ctx1]
  : EventCalcCtx[S1, E1, Ctx & Ctx1] =
    this.asInstanceOf[EventCalcCtx[S1, E1, Ctx & Ctx1]]


object EventCalcCtx:

  private val Empty: EventCalcCtx[?, ?, ?] =
    EventCalcCtx(Right(_))

  inline def empty[S <: EventDrivenState_[S, E], E <: Event, Ctx]: EventCalcCtx[S, E, Ctx] =
    Empty.asInstanceOf

  inline def apply[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    inline function: EventCollCtx[S, E, Ctx] => Checked[EventCollCtx[S, E, Ctx]])
  : EventCalcCtx[S, E, Ctx] =
    new EventCalcCtx(function)

  def problem[S <: EventDrivenState_[S, E], E <: Event, Ctx](problem: Problem): EventCalcCtx[S, E, Ctx] =
    EventCalcCtx(_ => Left(problem))

  inline def pure[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    keyedEvent: MaybeTimestampedKeyedEvent[E])
  : EventCalcCtx[S, E, Ctx] =
    pureEvent(keyedEvent)

  inline def pure[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    inline keyedEvents: MaybeTimestampedKeyedEvent[E]*)
  : EventCalcCtx[S, E, Ctx] =
    pureEvents(keyedEvents)

  inline def pure[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    inline keyedEvents: IterableOnce[MaybeTimestampedKeyedEvent[E]])
  : EventCalcCtx[S, E, Ctx] =
    pureEvents(keyedEvents)

  def pureNoKey[S <: EventDrivenState_[S, E], E <: NoKeyEvent, Ctx](events: Iterable[E])
  : EventCalcCtx[S, E, Ctx] =
    val eagerlyComputedKeyedEvents = events.view.map(NoKey <-: _).toVector
    EventCalcCtx(_.add(eagerlyComputedKeyedEvents))

  def pureEvent[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    keyedEvent: MaybeTimestampedKeyedEvent[E])
  : EventCalcCtx[S, E, Ctx] =
    EventCalcCtx(_.addEvent(keyedEvent))

  private def pureEvents[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    keyedEvents: IterableOnce[MaybeTimestampedKeyedEvent[E]])
  : EventCalcCtx[S, E, Ctx] =
    val eagerlyComputedKeyedEvents = keyedEvents.toEagerSeq
    EventCalcCtx(_.addEvents(eagerlyComputedKeyedEvents))

  def single[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    toKeyedEvent: S => OpaqueEventCollCtx[S, E, Ctx] ?=> KeyedEvent[E])
  : EventCalcCtx[S, E, Ctx] =
    EventCalcCtx: coll =>
      coll.addEvent:
        toKeyedEvent(coll.aggregate)(using coll)

  inline def maybe[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    inline toKeyedEvents: S => OpaqueEventCollCtx[S, E, Ctx] ?=> Option[MaybeTimestampedKeyedEvent[E]])
  : EventCalcCtx[S, E, Ctx] =
    multiple(toKeyedEvents)

  def checked[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    toCheckedKeyedEvents: S => OpaqueEventCollCtx[S, E, Ctx] ?=> Checked[IterableOnce[KeyedEvent[E]]])
  : EventCalcCtx[S, E, Ctx] =
    EventCalcCtx: coll =>
      coll.addChecked:
        toCheckedKeyedEvents(coll.aggregate)(using coll)

  def addChecked[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    keyedEvents: Checked[IterableOnce[KeyedEvent[E]]])
  : EventCalcCtx[S, E, Ctx] =
    val eagerlyComputedKeyedEvents = keyedEvents.map(_.toEagerSeq)
    EventCalcCtx(_.addChecked(eagerlyComputedKeyedEvents))

  def multiple[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    toKeyedEvents: S => OpaqueEventCollCtx[S, E, Ctx] ?=> IterableOnce[MaybeTimestampedKeyedEvent[E]])
  : EventCalcCtx[S, E, Ctx] =
    EventCalcCtx: coll =>
      coll.addEvents:
        toKeyedEvents(coll.aggregate)(using coll)

  opaque type OpaqueEventCollCtx[S <: EventDrivenState_[S, E], E <: Event, Ctx] =
    EventCollCtx[S, E, Ctx]

  def context[S <: EventDrivenState_[S, E], E <: Event, Ctx](
                                                              using coll: OpaqueEventCollCtx[S, E, Ctx])
  : Ctx =
    coll.context

  // Monoid //

  private sealed trait SomeAggregate extends EventDrivenState_[SomeAggregate, Event]

  private val GenericMonoid: Monoid[EventCalcCtx[SomeAggregate, Event, Any]] =
    type MyEventCalcCtx = EventCalcCtx[SomeAggregate, Event, Any]
    new Monoid[MyEventCalcCtx]:
      def empty = Empty.asInstanceOf[MyEventCalcCtx]

      def combine(a: MyEventCalcCtx, b: MyEventCalcCtx): MyEventCalcCtx =
        EventCalcCtx: coll =>
          a.calculate(coll).flatMap(b.calculate)

      // This implementation of combineAll avoids a hidden flatMap recursion
      override def combineAll(as: IterableOnce[MyEventCalcCtx]): MyEventCalcCtx =
        type MyEventColl = EventCollCtx[SomeAggregate, Event, Any]
        EventCalcCtx[SomeAggregate, Event, Any]: coll =>
          val it = as.iterator

          @tailrec def loop(coll: MyEventColl): Checked[MyEventColl] =
            if !it.hasNext then
              Right(coll)
            else
              it.next().function(coll) match
                case Left(problem) => Left(problem)
                case Right(coll2) => loop(coll2)

          loop(coll)

  given monoid: [S <: EventDrivenState_[S, E], E <: Event, Ctx] => Monoid[EventCalcCtx[S, E, Ctx]] =
    GenericMonoid.asInstanceOf

  def combineAll[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    eventCalcs: IterableOnce[EventCalcCtx[S, E, Ctx]])
  : EventCalcCtx[S, E, Ctx] =
    monoid.combineAll(eventCalcs)



/** A `EventCalcCtx` with a `TimeCtx`.
  * <p>
  * This is the regulary used Ctx type-parameter.
  */
type EventCalc[S <: EventDrivenState_[S, E], E <: Event] =
  EventCalcCtx[S, E, TimeCtx]


object EventCalc:

  inline def empty[S <: EventDrivenState_[S, E], E <: Event]: EventCalc[S, E] =
    EventCalcCtx.empty

  inline def apply[S <: EventDrivenState_[S, E], E <: Event](
    inline function: EventColl[S, E] => Checked[EventColl[S, E]])
  : EventCalc[S, E] =
    EventCalcCtx(function)

  inline def problem[S <: EventDrivenState_[S, E], E <: Event](inline problem: Problem)
  : EventCalc[S, E] =
    EventCalcCtx.problem(problem)

  inline def pure[S <: EventDrivenState_[S, E], E <: Event](
    keyedEvent: MaybeTimestampedKeyedEvent[E])
  : EventCalc[S, E] =
    EventCalcCtx.pure(keyedEvent)

  inline def pure[S <: EventDrivenState_[S, E], E <: Event](
    inline keyedEvents: MaybeTimestampedKeyedEvent[E]*)
  : EventCalc[S, E] =
    EventCalcCtx.pure(keyedEvents)

  inline def pure[S <: EventDrivenState_[S, E], E <: Event](
    inline keyedEvents: IterableOnce[MaybeTimestampedKeyedEvent[E]])
  : EventCalc[S, E] =
    EventCalcCtx.pure(keyedEvents)

  inline def pureNoKey[S <: EventDrivenState_[S, E], E <: NoKeyEvent](
    inline events: Iterable[E])
  : EventCalc[S, E] =
    EventCalcCtx.pureNoKey(events)

  inline def pureEvent[S <: EventDrivenState_[S, E], E <: Event](
    inline keyedEvent: MaybeTimestampedKeyedEvent[E])
  : EventCalc[S, E] =
    EventCalcCtx.pureEvent(keyedEvent)

  inline def single[S <: EventDrivenState_[S, E], E <: Event](
    inline toKeyedEvent: S => OpaqueEventColl[S, E] ?=> KeyedEvent[E])
  : EventCalc[S, E] =
    EventCalcCtx.single(toKeyedEvent)

  inline def maybe[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    inline toKeyedEvents: S => OpaqueEventCollCtx[S, E, Ctx] ?=> Option[MaybeTimestampedKeyedEvent[E]])
  : EventCalcCtx[S, E, Ctx] =
    EventCalcCtx.maybe(toKeyedEvents)

  inline def checked[S <: EventDrivenState_[S, E], E <: Event](
    inline toCheckedKeyedEvents:
      S => OpaqueEventColl[S, E] ?=> Checked[IterableOnce[KeyedEvent[E]]])
  : EventCalc[S, E] =
    EventCalcCtx.checked(toCheckedKeyedEvents)

  inline def addChecked[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    inline keyedEvents: Checked[IterableOnce[KeyedEvent[E]]])
  : EventCalcCtx[S, E, Ctx] =
    EventCalcCtx.addChecked(keyedEvents)

  inline def multiple[S <: EventDrivenState_[S, E], E <: Event](
    inline toKeyedEvents: S => OpaqueEventColl[S, E] ?=> IterableOnce[MaybeTimestampedKeyedEvent[E]])
  : EventCalc[S, E] =
    EventCalcCtx.multiple(toKeyedEvents)

  opaque type OpaqueEventColl[S <: EventDrivenState_[S, E], E <: Event] =
    EventCalcCtx.OpaqueEventCollCtx[S, E, TimeCtx]

  def context[S <: EventDrivenState_[S, E], E <: Event](using coll: OpaqueEventColl[S, E])
  : TimeCtx =
    EventCalcCtx.context

  def now[S <: EventDrivenState_[S, E], E <: Event](using coll: OpaqueEventColl[S, E])
  : Timestamp =
    EventCalcCtx.context.now

  /** TODO Don't use this, use `now`. */
  inline def clock[S <: EventDrivenState_[S, E], E <: Event](
    using coll: OpaqueEventColl[S, E])
  : WallClock =
    context.clock

  inline def combineAll[S <: EventDrivenState_[S, E], E <: Event](
    inline eventCalcs: IterableOnce[EventCalc[S, E]])
  : EventCalc[S, E] =
    EventCalcCtx.combineAll(eventCalcs)
