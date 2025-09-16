package js7.data.event

import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, foreachWithBracket, toEagerSeq}
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.event.EventColl.*
import js7.data.event.KeyedEvent.NoKey
import scala.reflect.ClassTag

/** EventDrivenState event collection.
  * <p>
  * Collects KeyedEvents while applying them to an EventDrivenState `S`.
  */
final case class EventColl[S <: EventDrivenState[S, E], E <: Event, Ctx] private(
  private val originalAggregate_ : S,
  timestampedKeyedEvents: Vector[MaybeTimestampedKeyedEvent[E]],
  aggregate: S,
  context: Ctx):

  /** Using originalAggregate probably means duplicate event application. */
  inline def originalAggregate: S =
    originalAggregate_

  inline def hasEvents: Boolean =
    timestampedKeyedEvents.nonEmpty

  inline def add[E1 <: E](keyedEvent: KeyedEvent[E1]): Checked[EventColl[S, E, Ctx]] =
    addEvent(keyedEvent)

  inline def add[E1 <: E](keyedEvents: IterableOnce[MaybeTimestampedKeyedEvent[E1]])
  : Checked[EventColl[S, E, Ctx]] =
    addEvents(keyedEvents)

  inline def add(keyedEvents: Checked[IterableOnce[MaybeTimestampedKeyedEvent[E]]]): Checked[EventColl[S, E, Ctx]] =
    addChecked(keyedEvents)

  inline def add[K, E1 <: E](key: K)(events: Iterable[E1])
    (using /*erased*/ E1: Event.KeyCompanion[? >: E1])
    (using /*erased*/ ev: K =:= E1.Key)
  : Checked[EventColl[S, E, Ctx]] =
    addWithKey[K, E1](key)(events)

  inline def add[E1 <: E, Ctx1 >: Ctx](eventCalc: EventCalc[S, E1, Ctx1])
  : Checked[EventColl[S, E, Ctx]] =
    addEventCalc(eventCalc)

  inline def add(other: EventColl[S, E, Ctx]): Checked[EventColl[S, E, Ctx]] =
    addColl(other)

  def addEvent[E1 <: E](keyedEvent: MaybeTimestampedKeyedEvent[E1]): Checked[EventColl[S, E, Ctx]] =
    aggregate.applyKeyedEvent(keyedEvent.keyedEvent).map: updated =>
      update(keyedEvent :: Nil, updated)

  def addChecked(keyedEvents: Checked[IterableOnce[MaybeTimestampedKeyedEvent[E]]])
  : Checked[EventColl[S, E, Ctx]] =
    keyedEvents.flatMap(addEvents)

  def addWithKey[K, E1 <: E](key: K)(events: Iterable[E1])
    (using /*erased*/ E1: Event.KeyCompanion[? >: E1])
    (using /*erased*/ ev: K =:= E1.Key)
  : Checked[EventColl[S, E, Ctx]] =
    addEvents:
      events.view.map: event =>
        KeyedEvent.any(key, event).asInstanceOf[KeyedEvent[E]]

  def addNoKey[E1 <: E](events: Iterable[E1])(using E1 <:< NoKeyEvent)
  : Checked[EventColl[S, E, Ctx]] =
    addEvents:
      events.view.map: event =>
        KeyedEvent.any(NoKey, event).asInstanceOf[KeyedEvent[E]]

  def addEvents(keyedEvents: IterableOnce[MaybeTimestampedKeyedEvent[E]])
  : Checked[EventColl[S, E, Ctx]] =
    val eventSeq = keyedEvents.toEagerSeq
    if eventSeq.isEmpty then
      Right(this)
    else
      aggregate.applyKeyedEvents(eventSeq.view.map(_.keyedEvent)).map: updated =>
        copy(
          timestampedKeyedEvents = timestampedKeyedEvents ++ eventSeq,
          aggregate = updated)

  def addEventCalc[E1 <: E, Ctx1 >: Ctx](eventCalc: EventCalc[S, E1, Ctx1])
  : Checked[EventColl[S, E, Ctx]] =
    // Call function with forwarded (events omitted) EventColl,
    // then add the resulting EventColl to this.
    eventCalc.widen[S, E, Ctx]
      .calculate(forward)
      .flatMap(addColl)

  def addColl(other: EventColl[S, E, Ctx]): Checked[EventColl[S, E, Ctx]] =
    if other.originalAggregate_ ne aggregate then
      // This should not happen, despite it is returned as a Problem
      val problem = Problem.pure("EventColl.addColl: coll.originalAggregate doesn't match aggregate")
      logger.warn(s"EventColl.addColl: coll.originalAggregate doesn't match aggregate",
        new Exception(problem.toString))
      aggregate.emitLineStream(o => logger.warn(s"aggregate=$o"))
      other.originalAggregate_.emitLineStream(o => logger.warn(s"other.originalAggregate=$o"))
      other.aggregate.emitLineStream(o => logger.warn(s"other.aggregate=$o"))
      Left(problem)
    else
      Right:
        if !other.hasEvents then
          this
        else if !hasEvents then
          other
        else
          update(other.timestampedKeyedEvents, other.aggregate)

  /** Collect events calculated from `iterable` and fail-fast with first problem. */
  def iterate[A](iterable: IterableOnce[A])
    (toColl: (EventColl[S, E, Ctx], A) => Checked[EventColl[S, E, Ctx]])
  : Checked[EventColl[S, E, Ctx]] =
    var coll = this
    val it = iterable.iterator
    while it.hasNext do
      toColl(coll, it.next()) match
        case Left(problem) => return Left(problem)
        case Right(o) => coll = o
    Right(coll)

  private def update(keyedEvents: Seq[MaybeTimestampedKeyedEvent[E]], updatedAggregate: S)
  : EventColl[S, E, Ctx] =
    if keyedEvents.isEmpty then
      assertThat(updatedAggregate eq aggregate)
      this
    else
      if isIntelliJIdea then ()//foreachLog(logger.trace)
      copy(
        timestampedKeyedEvents = timestampedKeyedEvents ++ keyedEvents,
        aggregate = updatedAggregate)

  def foreachLog(body: String => Unit): Unit =
    keyedEvents.foreachWithBracket(): (ke, br) =>
      body(s"$br$ke".trim)

  def keyedEvents: Vector[KeyedEvent[E]] =
    timestampedKeyedEvents.map(_.keyedEvent)

  /** Set originalAggregate to aggregate and remove the events. */
  def forward: EventColl[S, E, Ctx] =
    EventColl[S, E, Ctx](aggregate, context)

  def ifIs[S1 <: EventDrivenState[S1, E]](using ClassTag[S1]): Option[EventColl[S1, E, Ctx]] =
    implicitClass[S1].isAssignableFrom(aggregate.getClass) ?
      this.asInstanceOf[EventColl[S1,E, Ctx]]

  def widen[S1  <: EventDrivenState[S1, ? >: E1], E1 >: E <: Event, Ctx1 <: Ctx]
  : EventColl[S1, E1, Ctx] =
    this.asInstanceOf[EventColl[S1,E1, Ctx]]

  override def toString =
    s"EventColl[${aggregate.companion.name}](${timestampedKeyedEvents.size} events)"


object EventColl:
  private val logger = Logger[this.type]

  def apply[S <: EventDrivenState[S, E], E <: Event](aggregate: S): EventColl[S, E, Unit] =
    apply(aggregate, ())

  def apply[S <: EventDrivenState[S, E], E <: Event, Ctx](aggregate: S, context: Ctx)
  : EventColl[S, E, Ctx] =
    new EventColl(aggregate, Vector.empty, aggregate, context)

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

  def checkEvents[S <: EventDrivenState[S, E], E <: Event, Ctx](aggregate: S, context: Ctx)
    (keyedEvents: IterableOnce[KeyedEvent[E]])
  : Checked[Vector[KeyedEvent[E]]] =
    EventColl.keyedEvents(aggregate, context)(_.add(keyedEvents))

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
