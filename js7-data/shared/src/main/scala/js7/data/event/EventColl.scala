package js7.data.event

import js7.base.log.Logger
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp
import js7.base.utils.Assertions.{assertIfStrict, assertThat}
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichJavaClass, foreachWithBracket, mkStringLimited, toEagerSeq}
import js7.base.utils.Tests.isIntelliJIdea
import js7.data.event.KeyedEvent.NoKey
import scala.annotation.targetName
import scala.collection.IndexedSeqView
import scala.reflect.ClassTag

/** EventDrivenState event collection.
  * <p>
  * Collects KeyedEvents while applying them to an EventDrivenState `S`.
  *
  * @tparam Ctx Callers immutable context. May be anything, for instance, `TimeCtx` or `Unit`.
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

  inline def eventCount: Int =
    timestampedKeyedEvents.size

  inline def apply[E1 <: E](keyedEvent: MaybeTimestampedKeyedEvent[E1])
  : Checked[EventCollCtx[S, E, Ctx]] =
    add(keyedEvent)

  @targetName("applyKeyedEventsVarargs")
  inline def apply[E1 <: E](keyedEvents: MaybeTimestampedKeyedEvent[E1]*)
  : Checked[EventCollCtx[S, E, Ctx]] =
    add(keyedEvents)

  inline def apply[E1 <: E](keyedEvents: IterableOnce[MaybeTimestampedKeyedEvent[E1]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    add(keyedEvents)

  @targetName("apply_Checked_Single")
  inline def apply(keyedEvent: Checked[MaybeTimestampedKeyedEvent[E]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    addChecked(keyedEvent)

  inline def apply(keyedEvents: Checked[IterableOnce[MaybeTimestampedKeyedEvent[E]]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    add(keyedEvents)

  inline def apply[K, E1 <: E](key: K)(events: Iterable[E1])
    (using /*erased*/ E1: Event.KeyCompanion[? >: E1])
    (using /*erased*/ ev: K =:= E1.Key)
  : Checked[EventCollCtx[S, E, Ctx]] =
    add(key)(events)

  inline def apply[E1 <: E, Ctx1 >: Ctx](eventCalc: EventCalcCtx[S, E1, Ctx1])
  : Checked[EventCollCtx[S, E, Ctx]] =
    add(eventCalc)

  @targetName("applyMaybeEventCalc")
  inline def apply[E1 <: E, Ctx1 >: Ctx](eventCalc: Option[EventCalcCtx[S, E1, Ctx1]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    add(eventCalc)

  inline def apply(other: EventCollCtx[S, E, Ctx]): Checked[EventCollCtx[S, E, Ctx]] =
    add(other)

  inline def add[E1 <: E](keyedEvent: MaybeTimestampedKeyedEvent[E1])
  : Checked[EventCollCtx[S, E, Ctx]] =
    addEvent(keyedEvent)

  @targetName("add_KeyedEvents_varargs")
  inline def add[E1 <: E](keyedEvents: MaybeTimestampedKeyedEvent[E1]*)
  : Checked[EventCollCtx[S, E, Ctx]] =
    addEvents(keyedEvents)

  inline def add[E1 <: E](keyedEvents: IterableOnce[MaybeTimestampedKeyedEvent[E1]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    addEvents(keyedEvents)

  @targetName("add_Checked_Single")
  inline def add(keyedEvent: Checked[MaybeTimestampedKeyedEvent[E]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    addChecked(keyedEvent)

  inline def add(keyedEvents: Checked[IterableOnce[MaybeTimestampedKeyedEvent[E]]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    addChecked(keyedEvents)

  inline def add[K, E1 <: E](key: K)(events: Iterable[E1])
    (using /*erased*/ E1: Event.KeyCompanion[? >: E1])
    (using /*erased*/ ev: K =:= E1.Key)
  : Checked[EventCollCtx[S, E, Ctx]] =
    addWithKey[K, E1](key)(events)

  inline def add[E1 <: E, Ctx1 >: Ctx](inline eventCalc: EventCalcCtx[S, E1, Ctx1])
  : Checked[EventCollCtx[S, E, Ctx]] =
    addEventCalc(eventCalc)

  inline def add(inline other: EventCollCtx[S, E, Ctx]): Checked[EventCollCtx[S, E, Ctx]] =
    addColl(other)

  @targetName("add_Checked_EventColl")
  inline def add(inline other: Checked[EventCollCtx[S, E, Ctx]]): Checked[EventCollCtx[S, E, Ctx]] =
    addCheckedColl(other)

  @targetName("addMaybeEventCalc")
  inline def add[E1 <: E, Ctx1 >: Ctx](eventCalc: Option[EventCalcCtx[S, E1, Ctx1]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    eventCalc.fold(nix)(addEventCalc)

  def addEvent[E1 <: E](keyedEvent: MaybeTimestampedKeyedEvent[E1]): Checked[EventCollCtx[S, E, Ctx]] =
    val keyedEvents = keyedEvent :: Nil
    logProblem(keyedEvents):
      logEvents(keyedEvents)
      aggregate.applyKeyedEvent(keyedEvent.keyedEvent).map: updated =>
        append(keyedEvents, updated)

  @targetName("addChecked_single")
  def addChecked(keyedEvent: Checked[MaybeTimestampedKeyedEvent[E]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    addChecked(keyedEvent.map(_ :: Nil))

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

  def addWithKey[K, E1 <: E](key: K)(events: Checked[Iterable[E1]])
    (using /*erased*/ E1: Event.KeyCompanion[? >: E1])
    (using /*erased*/ ev: K =:= E1.Key)
  : Checked[EventCollCtx[S, E, Ctx]] =
    addChecked:
      events.map:
        _.view.map: event =>
          KeyedEvent.any(key, event).asInstanceOf[KeyedEvent[E]]

  def addNoKey[E1 <: E](events: Iterable[E1])(using E1 <:< NoKeyEvent)
  : Checked[EventCollCtx[S, E, Ctx]] =
    addEvents:
      events.view.map: event =>
        KeyedEvent.any(NoKey, event).asInstanceOf[KeyedEvent[E]]

  def addEvents(keyedEvents: IterableOnce[MaybeTimestampedKeyedEvent[E]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    val eventSeq = keyedEvents.toEagerSeq
    logProblem(eventSeq):
      if eventSeq.isEmpty then
        Right(this)
      else
        logEvents(eventSeq)
        aggregate.applyKeyedEvents(eventSeq.view.map(_.keyedEvent)).map: updated =>
          append(eventSeq, updated)

  inline def addEventCalc[E1 <: E, Ctx1 >: Ctx](eventCalc: EventCalcCtx[S, E1, Ctx1])
  : Checked[EventCollCtx[S, E, Ctx]] =
    eventCalc.addTo(this)

  inline def addCheckedEventCalc[E1 <: E, Ctx1 >: Ctx](
    eventCalc: Checked[EventCalcCtx[S, E1, Ctx1]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    eventCalc.flatMap(_.addTo(this))

  def addCheckedColl(other: Checked[EventCollCtx[S, E, Ctx]]): Checked[EventCollCtx[S, E, Ctx]] =
    other.flatMap(addColl)

  def addColl(other: EventCollCtx[S, E, Ctx]): Checked[EventCollCtx[S, E, Ctx]] =
    if other.originalAggregate_ ne aggregate then
      val problem = Problem.pure:
        if other.originalAggregate eq originalAggregate then
          "❌ EventColl.addColl: both EventColls have the same originalAggregate (no addColl required?)"
        else
          "❌ EventColl.addColl: other.originalAggregate doesn't match this.aggregate"
      logAddCollDifference(other, problem)
      Left(problem)
    else
      Right:
        if !other.hasEvents then
          this
        else if !hasEvents then
          other
        else
          append(other.timestampedKeyedEvents, other.aggregate)

  /** Add nothing, return `this` `Coll` unchanged.
    */
  inline def nix: Right[Problem, this.type] =
    Right(this)

  /** Collect events calculated from `iterable` and fail-fast with first problem. */
  def fold[A](iterable: IterableOnce[A])
    (toColl: (EventCollCtx[S, E, Ctx], A) => Checked[EventCollCtx[S, E, Ctx]])
  : Checked[EventCollCtx[S, E, Ctx]] =
    var coll = this
    val it = iterable.iterator
    while it.hasNext do
      toColl(coll, it.next()) match
        case Left(problem) => return Left(problem)
        case Right(o) => coll = o
    Right(coll)

  //private def updateX(keyedEvents: Seq[MaybeTimestampedKeyedEvent[E]], body: => Checked[S])
  //: Checked[EventCollCtx[S, E, Ctx]] =
  //  logProblem(keyedEvents):
  //    body.map: updated =>
  //      append(keyedEvents, updated)

  private def append(
    keyedEvents: Seq[MaybeTimestampedKeyedEvent[E]],
    updatedAggregate: S)
  : EventCollCtx[S, E, Ctx] =
    if keyedEvents.isEmpty then
      assertThat(updatedAggregate eq aggregate)
      this
    else
      copy(
        timestampedKeyedEvents = timestampedKeyedEvents ++ keyedEvents,
        aggregate = updatedAggregate)

  private def logProblem[A](keyedEvents: Seq[MaybeTimestampedKeyedEvent[E]])(body: => Checked[A])
  : Checked[A] =
    body match
      case Left(problem) =>
        keyedEvents.foreachWithBracket(): (e, br) =>
          logger.debug(s"❓ $br${e.keyedEvent}")
        logger.debug(
          s"❓ EventColl[${aggregate.companion.name}]: $problem",
          new Exception(problem.toString))
        () // line for breakpoint
      case _ =>
    body

  private inline def logEvents(inline keyedEvents: Seq[MaybeTimestampedKeyedEvent[E]]): Unit =
    if false && isIntelliJIdea then
      logEvents2(keyedEvents)
    def logEvents2(keyedEvents: Seq[MaybeTimestampedKeyedEvent[E]]) =
      keyedEvents.foreachWithBracket()((ke, br) => logger.trace(s"🔸$br$ke".trim))

  private def logAddCollDifference(other: EventCollCtx[S, E, Ctx], problem: Problem): Unit =
    logger.error(problem.toString, new Exception(problem.toString))
    // BIG LOG
    aggregate.emitLineStream(o => logger.info(s"aggregate=$o"))
    foreachEventString(o => logger.info(s"this.keyedEvents: $o"))
    other.originalAggregate_.emitLineStream(o => logger.info(s"other.originalAggregate=$o"))
    other.foreachEventString(o => logger.info(s"other.keyedEvents: $o"))
    other.aggregate.emitLineStream(o => logger.info(s"other.aggregate=$o"))

  def foreachEventString(body: String => Unit): Unit =
    keyedEvents.foreachWithBracket(): (ke, br) =>
      body(s"$br$ke".trim)

  def keyedEvents: IndexedSeqView[KeyedEvent[E]] =
    timestampedKeyedEvents.view.map(_.keyedEvent)

  def keyedEventList: List[KeyedEvent[E]] =
    keyedEvents.toList

  /** Set originalAggregate to aggregate and remove the events. */
  def forward: EventCollCtx[S, E, Ctx] =
    if !hasEvents then
      this
    else
      EventCollCtx[S, E, Ctx](aggregate, context)

  /** Set originalAggregate to aggregate and remove the events.
    *
    * @tparam E1 can be any subtype of `Event`,
    *            because the returned `EventColl` has still no events.
    */
  def forwardAs[E1 <: E]: EventCollCtx[S, E1, Ctx] =
    if !hasEvents then
      this.asInstanceOf[EventCollCtx[S, E1, Ctx]]
    else
      new EventCollCtx[S, E1, Ctx](aggregate, Vector.empty, aggregate, context)

  /** Checks if this is a extension of `coll`, with more events than `coll`.
    * <p>
    *   Both EventColls must have the same originalAggregate.
    *   `coll` must start with the same events as `this`.
    */
  def hasMoreEventsThan(coll: EventCollCtx[S, E, Ctx]): Boolean =
    assertThat((originalAggregate eq coll.originalAggregate) && eventCount >= coll.eventCount)
    assertIfStrict(coll.keyedEvents.indices.forall(i => coll.keyedEvents(i) eq keyedEvents(i)))
    unsafeHasMoreEventsThan(coll)

  private[event] inline def unsafeHasMoreEventsThan(coll: EventCollCtx[S, E, Ctx]): Boolean =
    eventCount > coll.eventCount

  def ifIs[S1 <: EventDrivenState_[S1, E]](using ClassTag[S1]): Option[EventCollCtx[S1, E, Ctx]] =
    implicitClass[S1].isAssignableFrom(aggregate.getClass) ?
      this.asInstanceOf[EventCollCtx[S1,E, Ctx]]

  def widen[S1 <: EventDrivenState_[S1, ? >: E1], E1 >: E <: Event]
  : EventCollCtx[S1, E1, Ctx] =
    this.asInstanceOf[EventCollCtx[S1,E1, Ctx]]

  /** Try to down-cast S to S1. */
  def narrowAggregate[S1 <: EventDrivenState_[S1, E]: ClassTag]
  : Checked[EventColl[S1, E]] =
    if implicitClass[S1].isAssignableFrom(aggregate.getClass) then
      Right(this.asInstanceOf[EventColl[S1, E]])
    else
      Problem:
        s"EventColl: expected ${implicitClass[S1].shortClassName}, but it's a ${aggregate.companion}"

  override def toString =
    s"EventCollCtx[${aggregate.companion.name}](${
      timestampedKeyedEvents.view.map(_.toShortString).mkStringLimited(3)})"


object EventCollCtx:
  private val logger = Logger[EventColl.type]

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

  extension [S <: EventDrivenState_[S, E], E <: Event](eventColl: EventColl[S, E])
    def now: Timestamp =
      eventColl.context.now


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
    EventCollCtx(aggregate, TimeCtx(now))

  /** Returns a `EventCollCtx` with `context: Unit`. */
  inline def apply[S <: EventDrivenState_[S, E], E <: Event](inline aggregate: S)
  : EventCollCtx[S, E, Unit] =
    EventCollCtx(aggregate)
