package js7.data.event

import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, foreachWithBracket, mkStringLimited, toEagerSeq}
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.event.KeyedEvent.NoKey
import scala.collection.IndexedSeqView
import scala.reflect.ClassTag

/** EventDrivenState event collection.
  * <p>
  * Collects KeyedEvents while applying them to an EventDrivenState `S`.
  *
  * @tparam Ctx Users immutable context. May be Unit.
  */
final case class EventCollCtx[S <: EventDrivenState_[S, E], E <: Event, Ctx] private(
  private val originalAggregate_ : S,
  timestampedKeyedEvents: Vector[MaybeTimestampedKeyedEvent[E]],
  aggregate: S,
  context: Ctx):

  import EventCollCtx.*

  /** Using originalAggregate probably means duplicate event application. */
  inline def originalAggregate: S =
    originalAggregate_

  inline def hasEvents: Boolean =
    timestampedKeyedEvents.nonEmpty

  inline def add[E1 <: E](keyedEvent: MaybeTimestampedKeyedEvent[E1]): Checked[EventCollCtx[S, E, Ctx]] =
    addEvent(keyedEvent)

  inline def add[E1 <: E](keyedEvents: IterableOnce[MaybeTimestampedKeyedEvent[E1]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    addEvents(keyedEvents)

  inline def add(keyedEvents: Checked[IterableOnce[MaybeTimestampedKeyedEvent[E]]]): Checked[EventCollCtx[S, E, Ctx]] =
    addChecked(keyedEvents)

  inline def add[K, E1 <: E](key: K)(events: Iterable[E1])
    (using /*erased*/ E1: Event.KeyCompanion[? >: E1])
    (using /*erased*/ ev: K =:= E1.Key)
  : Checked[EventCollCtx[S, E, Ctx]] =
    addWithKey[K, E1](key)(events)

  inline def add[E1 <: E, Ctx1 >: Ctx](eventCalc: EventCalcCtx[S, E1, Ctx1])
  : Checked[EventCollCtx[S, E, Ctx]] =
    addEventCalc(eventCalc)

  inline def add(other: EventCollCtx[S, E, Ctx]): Checked[EventCollCtx[S, E, Ctx]] =
    addColl(other)

  def addEvent[E1 <: E](keyedEvent: MaybeTimestampedKeyedEvent[E1]): Checked[EventCollCtx[S, E, Ctx]] =
    aggregate.applyKeyedEvent(keyedEvent.keyedEvent).map: updated =>
      update(keyedEvent :: Nil, updated)

  def addChecked(keyedEvents: Checked[IterableOnce[MaybeTimestampedKeyedEvent[E]]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    keyedEvents.flatMap(addEvents)

  def addWithKey[K, E1 <: E](key: K)(events: Iterable[E1])
    (using /*erased*/ E1: Event.KeyCompanion[? >: E1])
    (using /*erased*/ ev: K =:= E1.Key)
  : Checked[EventCollCtx[S, E, Ctx]] =
    addEvents:
      events.view.map: event =>
        KeyedEvent.any(key, event).asInstanceOf[KeyedEvent[E]]

  def addNoKey[E1 <: E](events: Iterable[E1])(using E1 <:< NoKeyEvent)
  : Checked[EventCollCtx[S, E, Ctx]] =
    addEvents:
      events.view.map: event =>
        KeyedEvent.any(NoKey, event).asInstanceOf[KeyedEvent[E]]

  def addEvents(keyedEvents: IterableOnce[MaybeTimestampedKeyedEvent[E]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    val eventSeq = keyedEvents.toEagerSeq
    if eventSeq.isEmpty then
      Right(this)
    else
      aggregate.applyKeyedEvents(eventSeq.view.map(_.keyedEvent)).map: updated =>
        copy(
          timestampedKeyedEvents = timestampedKeyedEvents ++ eventSeq,
          aggregate = updated)

  def addEventCalc[E1 <: E, Ctx1 >: Ctx](eventCalc: EventCalcCtx[S, E1, Ctx1])
  : Checked[EventCollCtx[S, E, Ctx]] =
    eventCalc.calculate(this)

  def addColl(other: EventCollCtx[S, E, Ctx]): Checked[EventCollCtx[S, E, Ctx]] =
    if other.originalAggregate_ ne aggregate then
      // This should not happen, despite it is returned as a Problem
      val problem = Problem.pure("EventCollCtx.addColl: coll.originalAggregate doesn't match aggregate")
      logger.error(s"EventCollCtx.addColl: coll.originalAggregate doesn't match aggregate",
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
    (toColl: (EventCollCtx[S, E, Ctx], A) => Checked[EventCollCtx[S, E, Ctx]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    var coll = this
    val it = iterable.iterator
    while it.hasNext do
      toColl(coll, it.next()) match
        case Left(problem) => return Left(problem)
        case Right(o) => coll = o
    Right(coll)

  private def update(keyedEvents: Seq[MaybeTimestampedKeyedEvent[E]], updatedAggregate: S)
  : EventCollCtx[S, E, Ctx] =
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

  def keyedEvents: IndexedSeqView[KeyedEvent[E]] =
    timestampedKeyedEvents.view.map(_.keyedEvent)

  /** Set originalAggregate to aggregate and remove the events. */
  def forward: EventCollCtx[S, E, Ctx] =
    EventCollCtx[S, E, Ctx](aggregate, context)

  def ifIs[S1 <: EventDrivenState_[S1, E]](using ClassTag[S1]): Option[EventCollCtx[S1, E, Ctx]] =
    implicitClass[S1].isAssignableFrom(aggregate.getClass) ?
      this.asInstanceOf[EventCollCtx[S1,E, Ctx]]

  def widen[S1  <: EventDrivenState_[S1, ? >: E1], E1 >: E <: Event, Ctx1 <: Ctx]
  : EventCollCtx[S1, E1, Ctx] =
    this.asInstanceOf[EventCollCtx[S1,E1, Ctx]]

  override def toString =
    s"EventCollCtx[${aggregate.companion.name}](${
      timestampedKeyedEvents.view.map(_.toShortString).mkStringLimited(3)})"


object EventCollCtx:
  private val logger = Logger[this.type]

  def apply[S <: EventDrivenState_[S, E], E <: Event](aggregate: S): EventCollCtx[S, E, Unit] =
    apply(aggregate, ())

  def apply[S <: EventDrivenState_[S, E], E <: Event, Ctx](aggregate: S, context: Ctx)
  : EventCollCtx[S, E, Ctx] =
    new EventCollCtx(aggregate, Vector.empty, aggregate, context)

  //<editor-fold desc="// (Tried to let Scala derive the S type parameter for add, keyedEvents)">
  //def apply[S <: EventDrivenState_[S, E], E <: Event](aggregate: S): KeyedEventsApply[S, E] =
  //  new KeyedEventsApply[S, E](aggregate)
  //
  //final class KeyedEventsApply[S <: EventDrivenState_[S, E], E <: Event](private val aggregate: S)
  //extends AnyVal:
  //  def apply(): EventCollCtx[S, E, Unit] =
  //    new EventCollCtx[S, E, Unit](aggregate, Vector.empty, aggregate, ())
  //
  //  inline def add[E1 <: E](keyedEvent: KeyedEvent[E1]): Checked[EventCollCtx[S, E1, Any]] =
  //    EventCollCtx[S, E1, Any](aggregate, ()).add(keyedEvent)
  //
  //  def keyedEvents[E1 <: E](body: EventCollCtx[S, E1, Any] => Checked[EventCollCtx[S, E1, Any]]) =
  //    body(EventCollCtx(aggregate, ())).map(_.keyedEvents)
  //
  //  def keyedEvents[E1 <: E, Ctx](context: Ctx)
  //    (body: EventCollCtx[S, E, Ctx] => Checked[EventCollCtx[S, E1, Ctx]])
  //  : Checked[Vector[KeyedEvent[E1]]] =
  //    body(EventCollCtx(aggregate, context)).map(_.keyedEvents)
  //</editor-fold

  def checkEvents[S <: EventDrivenState_[S, E], E <: Event, Ctx](aggregate: S, context: Ctx)
    (keyedEvents: IterableOnce[KeyedEvent[E]])
  : Checked[Vector[KeyedEvent[E]]] =
    EventCollCtx.keyedEvents(aggregate, context)(_.add(keyedEvents))

  def keyedEvents[S <: EventDrivenState_[S, E], E <: Event](aggregate: S)
    (body: EventCollCtx[S, E, Any] => Checked[EventCollCtx[S, E, Any]])
  : Checked[Vector[KeyedEvent[E]]] =
    keyedEvents[S, E, Any](aggregate, ())(body)

  def keyedEvents[S <: EventDrivenState_[S, E], E <: Event, Ctx](
    aggregate: S,
    context: Ctx)
    (body: EventCollCtx[S, E, Ctx] => Checked[EventCollCtx[S, E, Ctx]])
  : Checked[Vector[KeyedEvent[E]]] =
    body(EventCollCtx(aggregate, context)).map(_.keyedEvents.toVector)


/** A `EventCollCtx` with a `TimeCtx`.
  * <p>
  * This is the regulary used Ctx type-parameter.
  */
type EventColl[S <: EventDrivenState_[S, E], E <: Event] =
  EventCollCtx[S, E, TimeCtx]


object EventColl:
  private val logger = Logger[this.type]

  def apply[S <: EventDrivenState_[S, E], E <: Event](aggregate: S, now: Timestamp)
  : EventColl[S, E] =
    EventColl(aggregate, TimeCtx(now))

  def apply[S <: EventDrivenState_[S, E], E <: Event](aggregate: S, ctx: TimeCtx)
  : EventColl[S, E] =
    EventCollCtx(aggregate, ctx)

  /** Returns a `EventCollCtx` with `context: Unit`. */
  inline def apply[S <: EventDrivenState_[S, E], E <: Event](inline aggregate: S)
  : EventCollCtx[S, E, Unit] =
    EventCollCtx(aggregate)

  def checkEvents[S <: EventDrivenState_[S, E], E <: Event](aggregate: S, ctx: TimeCtx)
    (keyedEvents: IterableOnce[KeyedEvent[E]])
  : Checked[Vector[KeyedEvent[E]]] =
    EventColl.keyedEvents(aggregate, ctx)(_.add(keyedEvents))

  def keyedEvents[S <: EventDrivenState_[S, E], E <: Event](aggregate: S, now: Timestamp)
    (body: EventColl[S, E] => Checked[EventColl[S, E]])
  : Checked[Vector[KeyedEvent[E]]] =
    keyedEvents(aggregate, TimeCtx(now))(body)

  def keyedEvents[S <: EventDrivenState_[S, E], E <: Event](aggregate: S, ctx: TimeCtx)
    (body: EventColl[S, E] => Checked[EventColl[S, E]])
  : Checked[Vector[KeyedEvent[E]]] =
    body(EventColl(aggregate, ctx)).map(_.keyedEvents.toVector)

  inline def keyedEvents[S <: EventDrivenState_[S, E], E <: Event](
    inline aggregate: S)
    (inline body: EventCollCtx[S, E, Any] => Checked[EventCollCtx[S, E, Any]])
  : Checked[Vector[KeyedEvent[E]]] =
    EventCollCtx.keyedEvents(aggregate)(body)
