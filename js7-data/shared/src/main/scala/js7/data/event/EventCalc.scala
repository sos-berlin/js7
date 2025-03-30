package js7.data.event

import cats.Monoid
import js7.base.problem.{Checked, Problem}

/** Calculater for EventColl.
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

  def fail[S <: EventDrivenState[S, E], E <: Event, Ctx](problem: Problem): EventCalc[S, E, Ctx] =
    EventCalc(_ => Left(problem))

  def addChecked[S <: EventDrivenState[S, E], E <: Event, Ctx](
    keyedEvents: Checked[IterableOnce[KeyedEvent[E]]])
  : EventCalc[S, E, Ctx] =
    EventCalc(_.addChecked(keyedEvents))

  inline def add[S <: EventDrivenState[S, E], E <: Event, Ctx](keyedEvent: KeyedEvent[E])
  : EventCalc[S, E, Ctx] =
    addEvent(keyedEvent)

  inline def add[S <: EventDrivenState[S, E], E <: Event, Ctx](keyedEvents: KeyedEvent[E]*)
  : EventCalc[S, E, Ctx] =
    addEvents(keyedEvents)

  inline def add[S <: EventDrivenState[S, E], E <: Event, Ctx](
    keyedEvents: IterableOnce[MaybeTimestampedKeyedEvent[E]])
  : EventCalc[S, E, Ctx] =
    addEvents(keyedEvents)

  //inline def add[K, E1 <: E](key: K)(events: Iterable[E1])
  //  (using /*erased*/ E1: Event.KeyCompanion[? >: E1])
  //  (using /*erased*/ ev: K =:= E1.Key)
  //: Checked[EventCalc[S, E, Ctx] =
  //  addWithKey[K, E1](key)(events)

  def addNoKey[S <: EventDrivenState[S, E], E <: Event, Ctx](events: Iterable[E])(using E <:< NoKeyEvent)
  : EventCalc[S, E, Ctx] =
    EventCalc(_.addNoKey(events))

  private def addEvent[S <: EventDrivenState[S, E], E <: Event, Ctx](keyedEvent: KeyedEvent[E])
  : EventCalc[S, E, Ctx] =
    EventCalc(_.addEvent(keyedEvent))

  private def addEvents[S <: EventDrivenState[S, E], E <: Event, Ctx](
    keyedEvents: IterableOnce[MaybeTimestampedKeyedEvent[E]])
  : EventCalc[S, E, Ctx] =
    EventCalc(_.addEvents(keyedEvents))

  //private val Monoid = new Monoid[EventCalc[?, ?, ?]]:
  //  def empty = EventCalc.empty
  //
  //  def combine(a: EventCalc[?, ?, ?], b: EventCalc[?, ?, ?]): EventCalc[?, ?, ?] =
  //    a.combine[?, ?, ?](b)

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
