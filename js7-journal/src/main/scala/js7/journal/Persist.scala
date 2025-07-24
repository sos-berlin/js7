package js7.journal

import js7.data.event.{Event, EventCalc, EventDrivenState, TimeCtx}
import scala.concurrent.duration.Deadline

/** Command to write and commit events to the Journal. */
final case class Persist[S <: EventDrivenState[S, E], E <: Event] private(
  eventCalc: EventCalc[S, E, TimeCtx],
  commitOptions: CommitOptions,
  since: Deadline)


object Persist:

  def apply[S <: EventDrivenState[S, E], E <: Event](
    options: CommitOptions = CommitOptions.default,
    since: Deadline = Deadline.now)
    (eventCalc: EventCalc[S, E, TimeCtx]): Persist[S, E] =
    new Persist(eventCalc, options, since)

  def apply[S <: EventDrivenState[S, E], E <: Event]
    (eventCalc: EventCalc[S, E, TimeCtx])
  : Persist[S, E] =
    new Persist(eventCalc, CommitOptions.default, Deadline.now)
