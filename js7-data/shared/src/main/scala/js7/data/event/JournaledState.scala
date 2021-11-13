package js7.data.event

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._

/** An EventDrivenState with EventId. An aggregate. */
trait JournaledState[S <: JournaledState[S]]
extends EventDrivenState[S, Event]
{
  this: S =>

  def companion: JournaledState.Companion[S]

  override def applyStampedEvents(stampedEvents: Iterable[Stamped[KeyedEvent[Event]]]): Checked[S] =
    if (stampedEvents.isEmpty)
      Right(this)
    else
      super.applyStampedEvents(stampedEvents)
        .map(_.withEventId(stampedEvents.last.eventId))

  def applyEvent(keyedEvent: KeyedEvent[Event]): Checked[S]

  def withEventId(eventId: EventId): S

  def eventId: EventId
}

object JournaledState
{
  final case class EventNotApplicableProblem(keyedEvent: KeyedEvent[Event], state: Any) extends Problem.Coded {
    def arguments = Map(
      "event" -> keyedEvent.toString.truncateWithEllipsis(100),
      "state" -> state.toString.truncateWithEllipsis(100))
  }

  trait Companion[S <: JournaledState[S]]
  extends EventDrivenState.Companion[S, Event]
  {
    type StateEvent <: Event

    implicit final val implicitJournalStateCompanion: Companion[S] =
      this

    implicit def keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event]
  }
}
