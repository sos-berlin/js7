package js7.data.event

import cats.Monoid
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.toEagerSeq
import js7.data.event
import js7.data.event.KeyedEvent.NoKey
import org.jetbrains.annotations.TestOnly
import scala.annotation.tailrec
import scala.collection.IndexedSeqView

/** Calculation for EventColl.
  *
  * In essence, a function `EventCollCtx => Checked[EventCollCtx]`
  */
final class EventCalcCtx[S <: EventDrivenState_[S, E], E <: Event, Ctx](
  private val function: EventCollCtx[S, E, Ctx] => Checked[EventCollCtx[S, E, Ctx]]):

  /** Return an EventCollCtx containing the calculated events and aggregate. */
  def calculate(aggregate: S, context: Ctx): Checked[EventCollCtx[S, E, Ctx]] =
    addTo[S, E, Ctx](EventCollCtx(aggregate, context))

  /** Return an EventCollCtx containing the calculated events and aggregate.
    * <p>Simply calls  `function` with the `forward`ed (emptied) `coll`.
    * <p>Try to avoid this method.
    * @see ifHasEventsAddToCollElse
    * @see addTo
    */
  def calculate[S1 >: S <: EventDrivenState_[S1, E1], E1 >: E <: Event, Ctx1](
    coll: EventCollCtx[S1, E1, Ctx & Ctx1])
  : Checked[EventCollCtx[S1, E1, Ctx & Ctx1]] =
    addTo[S1, E1, Ctx1](coll.forward/*exclude events from the result*/)

  /** Return an EventCollCtx containing the calculated events and aggregate.
    * <p>Simply calls `function` with the provided `coll`.
    * The calculated events are appended to `coll`.
    * <p>
    * If you want only the calculated events, use `calculateEvents`.
    * If `coll` is not a `forward`ed `EventCollCtx`,
    * the result contains the events from `coll` and the calculated events appended.
    */
  inline def addTo[S1 >: S <: EventDrivenState_[S1, E1], E1 >: E <: Event, Ctx1](
    coll: EventCollCtx[S1, E1, Ctx & Ctx1])
  : Checked[EventCollCtx[S1, E1, Ctx & Ctx1]] =
    widen[S1, E1, Ctx & Ctx1].function(coll)

  inline def |+|(other: EventCalcCtx[S, E, Ctx]): EventCalcCtx[S, E, Ctx] =
    append(other)

  def append(other: EventCalcCtx[S, E, Ctx]): EventCalcCtx[S, E, Ctx] =
    EventCalcCtx: coll =>
      calculate(coll).flatMap: myColl =>
        myColl.addEventCalc(other)

  @TestOnly // Seems not to be typesafe
  def untypedCalculateEventList(aggregate: EventDrivenState[E], ctx: Ctx)
  : Checked[List[KeyedEvent[E]]] =
    calculateEventList:
      EventCollCtx(aggregate.asInstanceOf[S], ctx)

  @TestOnly
  def calculateEventList(coll: EventCollCtx[S, E, Ctx]): Checked[List[KeyedEvent[E]]] =
    calculateEvents(coll).map(_.toList)

  /** Return the calculated events.
    * <p>See `calculate`.
    */
  def calculateEvents(coll: EventCollCtx[S, E, Ctx]): Checked[IndexedSeqView[KeyedEvent[E]]] =
    calculate[S, E, Ctx](coll).map(_.keyedEvents)

  /** Add this to coll or, if coll has no events, execute the body
    */
  def ifHasEventsAddToCollElse(coll: EventCollCtx[S, E, Ctx])
    (whenNoEvents: => Checked[EventCollCtx[S, E, Ctx]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    addTo(coll).flatMap: newColl =>
      if newColl.unsafeHasMoreEventsThan(coll) then
        Right(newColl)
      else
        whenNoEvents

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
    toKeyedEvent: S => KeyedEvent[E])
  : EventCalcCtx[S, E, Ctx] =
    EventCalcCtx: coll =>
      coll.addEvent:
        toKeyedEvent(coll.aggregate)

  inline def maybe[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    inline toKeyedEvents: S => Option[MaybeTimestampedKeyedEvent[E]])
  : EventCalcCtx[S, E, Ctx] =
    multiple(toKeyedEvents)

  def checked[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    toCheckedKeyedEvents: S => Checked[IterableOnce[KeyedEvent[E]]])
  : EventCalcCtx[S, E, Ctx] =
    EventCalcCtx: coll =>
      coll.addChecked:
        toCheckedKeyedEvents(coll.aggregate)

  def addChecked[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    keyedEvents: Checked[IterableOnce[KeyedEvent[E]]])
  : EventCalcCtx[S, E, Ctx] =
    val eagerlyComputedKeyedEvents = keyedEvents.map(_.toEagerSeq)
    EventCalcCtx(_.addChecked(eagerlyComputedKeyedEvents))

  def multiple[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    toKeyedEvents: S => IterableOnce[MaybeTimestampedKeyedEvent[E]])
  : EventCalcCtx[S, E, Ctx] =
    EventCalcCtx: coll =>
      coll.addEvents:
        toKeyedEvents(coll.aggregate)

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
        a.append(b)

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

  // require Ctx type parameter, but Ctx may be Any, so problem and pure are more flexible
  export EventCalcCtx.{problem, pure}
  //<editor-fold desc="// TimeCtx methods: problem, pure, ...">
  //inline def problem[S <: EventDrivenState_[S, E], E <: Event](inline problem: Problem)
  //: EventCalc[S, E] =
  //  EventCalcCtx.problem(problem)
  //
  //inline def pure[S <: EventDrivenState_[S, E], E <: Event](
  //  keyedEvent: MaybeTimestampedKeyedEvent[E])
  //: EventCalc[S, E] =
  //  EventCalcCtx.pure(keyedEvent)
  //
  //inline def pure[S <: EventDrivenState_[S, E], E <: Event](
  //  inline keyedEvents: MaybeTimestampedKeyedEvent[E]*)
  //: EventCalc[S, E] =
  //  EventCalcCtx.pure(keyedEvents)
  //
  //inline def pure[S <: EventDrivenState_[S, E], E <: Event](
  //  inline keyedEvents: IterableOnce[MaybeTimestampedKeyedEvent[E]])
  //: EventCalc[S, E] =
  //  EventCalcCtx.pure(keyedEvents)
  //
  //inline def pureNoKey[S <: EventDrivenState_[S, E], E <: NoKeyEvent](
  //  inline events: Iterable[E])
  //: EventCalc[S, E] =
  //  EventCalcCtx.pureNoKey(events)
  //
  //inline def pureEvent[S <: EventDrivenState_[S, E], E <: Event](
  //  inline keyedEvent: MaybeTimestampedKeyedEvent[E])
  //: EventCalc[S, E] =
  //  EventCalcCtx.pureEvent(keyedEvent)
  //</editor-fold>

  inline def single[S <: EventDrivenState_[S, E], E <: Event](
    inline toKeyedEvent: S => KeyedEvent[E])
  : EventCalc[S, E] =
    EventCalcCtx.single(toKeyedEvent)

  inline def maybe[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    inline toKeyedEvents: S => Option[MaybeTimestampedKeyedEvent[E]])
  : EventCalcCtx[S, E, Ctx] =
    EventCalcCtx.maybe(toKeyedEvents)

  inline def checked[S <: EventDrivenState_[S, E], E <: Event](
    inline toCheckedKeyedEvents:
      S => Checked[IterableOnce[KeyedEvent[E]]])
  : EventCalc[S, E] =
    EventCalcCtx.checked(toCheckedKeyedEvents)

  inline def addChecked[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    inline keyedEvents: Checked[IterableOnce[KeyedEvent[E]]])
  : EventCalcCtx[S, E, Ctx] =
    EventCalcCtx.addChecked(keyedEvents)

  inline def multiple[S <: EventDrivenState_[S, E], E <: Event](
    inline toKeyedEvents: S => IterableOnce[MaybeTimestampedKeyedEvent[E]])
  : EventCalc[S, E] =
    EventCalcCtx.multiple(toKeyedEvents)

  inline def combineAll[S <: EventDrivenState_[S, E], E <: Event](
    inline eventCalcs: IterableOnce[EventCalc[S, E]])
  : EventCalc[S, E] =
    EventCalcCtx.combineAll(eventCalcs)
