package js7.data.event

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.Problems.OrderCannotAttachedToPlanProblem
import scala.util.boundary

/** An EventDrivenState with EventId. An aggregate. */
trait JournaledState[S <: JournaledState[S]]
extends EventDrivenState[S, Event]:
  this: S =>

  def companion: JournaledState.Companion[S]

  protected def withEventId_(eventId: EventId): S

  final def withEventId(eventId: EventId): S =
    if eventId == this.eventId then
      this
    else
      withEventId_(eventId)

  def eventId: EventId

  final def applyStampedEvents(stampedEvents: Iterable[Stamped[KeyedEvent[Event]]]): Checked[S] =
    if stampedEvents.isEmpty then
      Right(this)
    else
      // Duplicate with applyEvents to allow to in include EventId in error message
      var state = this
      var problem: Problem | Null = null
      var lastEventId = EventId.BeforeFirst

      boundary:
        for stamped <- stampedEvents.iterator do
          lastEventId = stamped.eventId
          state.applyKeyedEvent(stamped.value) match
            case Left(prblm) =>
              problem = prblm match
                case OrderCannotAttachedToPlanProblem(orderId, _)
                  if stamped.value.key == orderId =>
                  prblm
                case _ =>
                  prblm |+| Problem(s"Event '$stamped' could not be applied to ${companion.name}")
              boundary.break()
            case Right(s) =>
              state = s

      problem.toLeftOr(state.withEventId(lastEventId))


object JournaledState:

  trait HasEventCodec:
    implicit def keyedEventJsonCodec: KeyedEventTypedJsonCodec[Event]

  trait Companion[S <: JournaledState[S]]
  extends EventDrivenState.Companion[S], HasEventCodec:

    given journalStateCompanion: Companion[S] = this
