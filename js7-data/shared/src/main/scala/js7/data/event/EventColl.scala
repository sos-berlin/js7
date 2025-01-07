package js7.data.event

import js7.base.problem.{Checked, Problem}
import js7.base.utils.Tests.isStrict

/** EventDrivenState event collection.
  * <p>
  * Collects KeyedEvents while applying them to an EventDrivenState `S`.
  */
final case class EventColl[S  <: EventDrivenState[S, E], E <: Event] private(
  originalAggregate: S,
  keyedEvents: Vector[KeyedEvent[E]],
  aggregate: S):

  type Aggregate = S

  def computeEvent(toEvent: S => Checked[KeyedEvent[E]]): Checked[EventColl[S, E]] =
    toEvent(aggregate).flatMap(addEvent)

  def computeEvents(toEvents: S => Checked[IterableOnce[KeyedEvent[E]]]): Checked[EventColl[S, E]] =
    toEvents(aggregate).flatMap(addEvents)

  def add(keyedEvent: KeyedEvent[E]): Checked[EventColl[S, E]] =
    addEvent(keyedEvent)

  def add(keyedEvents: IterableOnce[KeyedEvent[E]]): Checked[EventColl[S, E]] =
    addEvents(keyedEvents)

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
      Left(Problem(s"EventColl.append: Aggregates don't match"))
    else
      Right:
        copy(
          keyedEvents = keyedEvents ++ b.keyedEvents,
          aggregate = b.aggregate)

  override def toString =
    s"EventColl[${aggregate.companion.name}](${keyedEvents.map(_.toShortString).mkString(", ")})"


object EventColl:
  def apply[S <: EventDrivenState[S, E], E <: Event](a: S): EventColl[S, E] =
    new EventColl[S, E](a, Vector.empty, a)
