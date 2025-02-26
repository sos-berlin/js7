package js7.data.event

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.base.utils.Tests.isStrict
import js7.data.event.KeyedEvent.NoKey
import scala.reflect.ClassTag

/** EventDrivenState event collection.
  * <p>
  * Collects KeyedEvents while applying them to an EventDrivenState `S`.
  */
final case class EventColl[S <: EventDrivenState[S, E], E <: Event] private(
  originalAggregate: S,
  keyedEvents: Vector[KeyedEvent[E]],
  aggregate: S):

  type Aggregate = S

  def computeEvent(toEvent: S => Checked[KeyedEvent[E]]): Checked[EventColl[S, E]] =
    toEvent(aggregate).flatMap(addEvent)

  def computeEvents(toEvents: S => Checked[IterableOnce[KeyedEvent[E]]]): Checked[EventColl[S, E]] =
    toEvents(aggregate).flatMap(addEvents)

  def addChecked(keyedEvents: Checked[IterableOnce[KeyedEvent[E]]]): Checked[EventColl[S, E]] =
    keyedEvents.flatMap(addEvents)

  inline def add(keyedEvent: KeyedEvent[E]): Checked[EventColl[S, E]] =
    addEvent(keyedEvent)

  inline def add(keyedEvents: IterableOnce[KeyedEvent[E]]): Checked[EventColl[S, E]] =
    addEvents(keyedEvents)

  inline def add[K, E1 <: E](key: K)(events: Iterable[E1])
    (using /*erased*/ E1: Event.KeyCompanion[? >: E1])
    (using /*erased*/ ev: K =:= E1.Key)
  : Checked[EventColl[S, E]] =
    addWithKey[K, E1](key)(events)

  def addWithKey[K, E1 <: E](key: K)(events: Iterable[E1])
    (using /*erased*/ E1: Event.KeyCompanion[? >: E1])
    (using /*erased*/ ev: K =:= E1.Key)
  : Checked[EventColl[S, E]] =
    addEvents:
      events.view.map: event =>
        KeyedEvent.any(key, event).asInstanceOf[KeyedEvent[E]]

  def addNoKey[K, E1 <: E](events: Iterable[E1])(using E1 <:< NoKeyEvent)
  : Checked[EventColl[S, E]] =
    addEvents:
      events.view.map: event =>
        KeyedEvent.any(NoKey, event).asInstanceOf[KeyedEvent[E]]

  def addEvent[E1 <: E](keyedEvent: KeyedEvent[E1]): Checked[EventColl[S, E]] =
    aggregate.applyKeyedEvent(keyedEvent).map: updated =>
      copy(
        keyedEvents = keyedEvents :+ keyedEvent,
        aggregate = updated)

  def addEvents(keyedEvents: IterableOnce[KeyedEvent[E]]): Checked[EventColl[S, E]] =
    val keyedEventsV = Vector.from(keyedEvents)
    if keyedEventsV.isEmpty then
      Right(this)
    else
      aggregate.applyKeyedEvents(keyedEventsV).map: updated =>
        copy(
          keyedEvents = this.keyedEvents ++ keyedEventsV,
          aggregate = updated)

  def append(b: EventColl[S, E]): Checked[EventColl[S, E]] =
    if isStrict && /*slow*/aggregate != b.originalAggregate then
      Left(Problem.pure("EventColl.append: Aggregates don't match"))
    else
      Right:
        copy(
          keyedEvents = keyedEvents ++ b.keyedEvents,
          aggregate = b.aggregate)

  def ifIs[S1 <: EventDrivenState[S1, E]](using S1: ClassTag[S1]): Option[EventColl[S1, E]] =
    implicitClass[S1].isAssignableFrom(aggregate.getClass) ?
      this.asInstanceOf[EventColl[S1, E]]

  private def widen[S1  <: EventDrivenState[S1, E1], E1 >: E <: Event]: EventColl[S1, E1] =
    this.asInstanceOf[EventColl[S1, E1]]

  override def toString =
    s"EventColl[${aggregate.companion.name}](${keyedEvents.map(_.toShortString).mkString(", ")})"


object EventColl:

  def apply[S <: EventDrivenState[S, E], E <: Event](a: S): EventColl[S, E] =
    new EventColl[S, E](a, Vector.empty, a)

  def checkEvents[S <: EventDrivenState[S, E], E <: Event](state: S)
    (keyedEvents: IterableOnce[KeyedEvent[E]])
  : Checked[Vector[KeyedEvent[E]]] =
    EventColl.keyedEvents(state)(_.add(keyedEvents))

  def keyedEvents[S <: EventDrivenState[S, E], E <: Event](
    state: S)
    (body: EventColl[S, E] => Checked[EventColl[S, E]])
  : Checked[Vector[KeyedEvent[E]]] =
    body(EventColl[S, E](state)).map(_.keyedEvents)
