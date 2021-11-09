package js7.data.event

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.{RichJavaClass, RichString}
import js7.data.event.EventDrivenState._

trait EventDrivenState[This <: EventDrivenState[This, E], E <: Event]
{
  this: This =>

  def companion: Companion[This, E]

  def applyEvent(keyedEvent: KeyedEvent[E]): Checked[This]

  def applyStampedEvents(stampedEvents: Iterable[Stamped[KeyedEvent[E]]]): Checked[This] = {
    var state = this
    var problem: Problem = null
    for (stamped <- stampedEvents.iterator if problem == null) {
      state.applyEvent(stamped.value) match {
        case Left(o) =>
          problem = o withPrefix s"Event '$stamped' cannot be applied to '${companion.name}':"
        case Right(s) =>
          state = s
      }
    }
    if (problem != null) Left(problem) else Right(state)
  }

  def applyEvents(keyedEvents: IterableOnce[KeyedEvent[E]]): Checked[This] = {
    var state = this
    var problem: Problem = null
    for (keyedEvent <- keyedEvents.iterator if problem == null) {
      state.applyEvent(keyedEvent) match {
        case Left(o) =>
          problem = o withPrefix s"Event '$keyedEvent' cannot be applied to '${companion.name}':"
        case Right(s) =>
          state = s
      }
    }
    if (problem != null) Left(problem) else Right(state)
  }

  protected final def eventNotApplicable(keyedEvent: KeyedEvent[Event]) =
    Left(EventNotApplicableProblem(keyedEvent, this))
}

object EventDrivenState
{
  trait Companion[S <: EventDrivenState[S, E], E <: Event]
  {
    implicit final val implicitEventDrivvenStateCompanion: Companion[S, E] =
      this

    def name: String =
      getClass.simpleScalaName

    override def toString = name
  }

  final case class EventNotApplicableProblem(keyedEvent: KeyedEvent[Event], state: Any) extends Problem.Coded {
    def arguments = Map(
      "event" -> keyedEvent.toString.truncateWithEllipsis(100),
      "state" -> state.toString.truncateWithEllipsis(100))
  }
}
