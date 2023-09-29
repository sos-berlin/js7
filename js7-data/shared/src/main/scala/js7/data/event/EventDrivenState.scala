package js7.data.event

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichString
import js7.data.event.EventDrivenState.*

trait EventDrivenState[Self <: EventDrivenState[Self, E], E <: Event] extends BasicState[Self]:
  this: Self =>

  override def companion: Companion[Self, E]

  def applyEvent(keyedEvent: KeyedEvent[E]): Checked[Self]

  def applyStampedEvents(stampedEvents: Iterable[Stamped[KeyedEvent[E]]]): Checked[Self] =
    var state = this
    var problem: Problem = null
    for stamped <- stampedEvents.iterator if problem == null do
      state.applyEvent(stamped.value) match
        case Left(o) =>
          problem = o withPrefix s"Event '$stamped' cannot be applied to '${companion.name}':"
        case Right(s) =>
          state = s
    if problem != null then Left(problem) else Right(state)

  def applyEvents(keyedEvents: IterableOnce[KeyedEvent[E]]): Checked[Self] =
    var state = this
    var problem: Problem = null
    for keyedEvent <- keyedEvents.iterator if problem == null do
      state.applyEvent(keyedEvent) match
        case Left(o) =>
          problem = o withPrefix s"Event '$keyedEvent' cannot be applied to '${companion.name}':"
        case Right(s) =>
          state = s
    if problem != null then Left(problem) else Right(state)

  protected final def eventNotApplicable(keyedEvent: KeyedEvent[Event]) =
    Left(EventNotApplicableProblem(keyedEvent, this))

object EventDrivenState:
  trait Companion[S <: EventDrivenState[S, E], E <: Event] extends BasicState.Companion[S]:
    implicit final val implicitEventDrivenStateCompanion: Companion[S, E] =
      this

    override def toString = name

  final case class EventNotApplicableProblem(keyedEvent: KeyedEvent[Event], state: Any) extends Problem.Coded:
    def arguments = Map(
      "event" -> keyedEvent.toString.truncateWithEllipsis(100),
      "state" -> state.toString.truncateWithEllipsis(100))
