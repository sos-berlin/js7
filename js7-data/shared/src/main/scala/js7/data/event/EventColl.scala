package js7.data.event

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.base.utils.Tests.isStrict
import js7.data.event.KeyedEvent.NoKey
import scala.reflect.ClassTag

/** EventDrivenState event collection.
  * <p>
  * Collects KeyedEvents while applying them to an EventDrivenState `S`.
  */
final case class EventColl[S <: EventDrivenState[S, E], E <: Event, Ctx] private(
  originalAggregate: S,
  keyedEvents: Vector[KeyedEvent[E]],
  aggregate: S,
  context: Ctx):

  private type Self = EventColl[S, E, Ctx]

  //def computeEvent(toEvent: S => Checked[KeyedEvent[E]]): Checked[Self] =
  //  toEvent(aggregate).flatMap(addEvent)
  //
  //def computeEvents(toEvents: S => Checked[IterableOnce[KeyedEvent[E]]]): Checked[Self] =
  //  toEvents(aggregate).flatMap(addEvents)

  def addChecked(keyedEvents: Checked[IterableOnce[KeyedEvent[E]]]): Checked[Self] =
    keyedEvents.flatMap(addEvents)

  inline def add[E1 <: E](keyedEvent: KeyedEvent[E1]): Checked[Self] =
    addEvent(keyedEvent)

  inline def add[E1 <: E](keyedEvents: IterableOnce[KeyedEvent[E1]]): Checked[Self] =
    addEvents(keyedEvents)

  inline def add[K, E1 <: E](key: K)(events: Iterable[E1])
    (using /*erased*/ E1: Event.KeyCompanion[? >: E1])
    (using /*erased*/ ev: K =:= E1.Key)
  : Checked[Self] =
    addWithKey[K, E1](key)(events)

  def addWithKey[K, E1 <: E](key: K)(events: Iterable[E1])
    (using /*erased*/ E1: Event.KeyCompanion[? >: E1])
    (using /*erased*/ ev: K =:= E1.Key)
  : Checked[Self] =
    addEvents:
      events.view.map: event =>
        KeyedEvent.any(key, event).asInstanceOf[KeyedEvent[E]]

  def addNoKey[E1 <: E](events: Iterable[E1])(using E1 <:< NoKeyEvent)
  : Checked[Self] =
    addEvents:
      events.view.map: event =>
        KeyedEvent.any(NoKey, event).asInstanceOf[KeyedEvent[E]]

  def addEvent[E1 <: E](keyedEvent: KeyedEvent[E1]): Checked[Self] =
    aggregate.applyKeyedEvent(keyedEvent).map: updated =>
      copy(
        keyedEvents = keyedEvents :+ keyedEvent,
        aggregate = updated)

  def addEvents(keyedEvents: IterableOnce[KeyedEvent[E]]): Checked[Self] =
    val keyedEventsV = Vector.from(keyedEvents)
    if keyedEventsV.isEmpty then
      Right(this)
    else
      aggregate.applyKeyedEvents(keyedEventsV).map: updated =>
        copy(
          keyedEvents = this.keyedEvents ++ keyedEventsV,
          aggregate = updated)

  def combine(b: Self): Checked[Self] =
    if isStrict && /*slow*/aggregate != b.originalAggregate then
      Left(Problem.pure("EventColl.combine: Aggregates don't match"))
    else
      Right:
        copy(
          keyedEvents = keyedEvents ++ b.keyedEvents,
          aggregate = b.aggregate)

  def ifIs[S1 <: EventDrivenState[S1, E]](using ClassTag[S1]): Option[EventColl[S1, E, Ctx]] =
    implicitClass[S1].isAssignableFrom(aggregate.getClass) ?
      this.asInstanceOf[EventColl[S1,E, Ctx]]

  def widen[S1  <: EventDrivenState[S1, ? >: E1], E1 >: E <: Event]: EventColl[S1, E1, Ctx] =
    this.asInstanceOf[EventColl[S1,E1, Ctx]]

  override def toString =
    s"EventColl[${aggregate.companion.name}](${keyedEvents.map(_.toShortString).mkString(", ")})"


object EventColl:
  def apply[S <: EventDrivenState[S, E], E <: Event](a: S): EventColl[S, E, Unit] =
    new EventColl(a, Vector.empty, a, ())

  def apply[S <: EventDrivenState[S, E], E <: Event, Ctx](a: S, context: Ctx)
  : EventColl[S, E, Ctx] =
    new EventColl(a, Vector.empty, a, context)

  //<editor-fold desc="// (Tried to let Scala derive the S type parameter for add, keyedEvents)">
  //def apply[S <: EventDrivenState[S, E], E <: Event](aggregate: S): KeyedEventsApply[S, E] =
  //  new KeyedEventsApply[S, E](aggregate)
  //
  //final class KeyedEventsApply[S <: EventDrivenState[S, E], E <: Event](private val aggregate: S)
  //extends AnyVal:
  //  def apply(): EventColl[S, E, Unit] =
  //    new EventColl[S, E, Unit](aggregate, Vector.empty, aggregate, ())
  //
  //  inline def add[E1 <: E](keyedEvent: KeyedEvent[E1]): Checked[EventColl[S, E1, Any]] =
  //    EventColl[S, E1, Any](aggregate, ()).add(keyedEvent)
  //
  //  def keyedEvents[E1 <: E](body: EventColl[S, E1, Any] => Checked[EventColl[S, E1, Any]]) =
  //    body(EventColl(aggregate, ())).map(_.keyedEvents)
  //
  //  def keyedEvents[E1 <: E, Ctx](context: Ctx)
  //    (body: EventColl[S, E, Ctx] => Checked[EventColl[S, E1, Ctx]])
  //  : Checked[Vector[KeyedEvent[E1]]] =
  //    body(EventColl(aggregate, context)).map(_.keyedEvents)
  //</editor-fold

  def checkEvents[S <: EventDrivenState[S, E], E <: Event, Ctx](state: S, context: Ctx)
    (keyedEvents: IterableOnce[KeyedEvent[E]])
  : Checked[Vector[KeyedEvent[E]]] =
    EventColl.keyedEvents(state, context)(_.add(keyedEvents))

  def keyedEvents[S <: EventDrivenState[S, E], E <: Event](aggregate: S)
    (body: EventColl[S, E, Any] => Checked[EventColl[S, E, Any]])
  : Checked[Vector[KeyedEvent[E]]] =
    keyedEvents[S, E, Any](aggregate, ())(body)

  def keyedEvents[S <: EventDrivenState[S, E], E <: Event, Ctx](
    aggregate: S,
    context: Ctx)
    (body: EventColl[S, E, Ctx] => Checked[EventColl[S, E, Ctx]])
  : Checked[Vector[KeyedEvent[E]]] =
    body(EventColl(aggregate, context)).map(_.keyedEvents)
