package js7.data.event

import js7.base.problem.Checked

/** An EventDrivenState with EventId. An aggregate. */
trait JournaledState[S <: JournaledState[S]]
extends EventDrivenState[S, Event]:
  this: S =>

  def companion: JournaledState.Companion[S]

  def withEventId(eventId: EventId): S

  def eventId: EventId

  override final def applyStampedEvents(stampedEvents: Iterable[Stamped[KeyedEvent[Event]]])
  : Checked[S] =
    if stampedEvents.isEmpty then
      Right(this)
    else
      super.applyStampedEvents(stampedEvents).map:
        _.withEventId(stampedEvents.last.eventId)


object JournaledState:

  trait HasEventCodec:
    implicit def keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event]

  trait Companion[S <: JournaledState[S]]
  extends EventDrivenState.Companion[S], HasEventCodec:

    given journalStateCompanion: Companion[S] = this
