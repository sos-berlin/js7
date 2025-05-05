package js7.journal

import js7.data.event.{Event, EventCalc, EventDrivenState, TimeCtx}
import scala.concurrent.duration.Deadline

/** Command to write and commit events to the Journal. */
final case class Persist[S <: EventDrivenState[S, E], E <: Event](
  eventCalc: EventCalc[S, E, TimeCtx],
  options: CommitOptions = CommitOptions.default,
  since: Deadline = Deadline.now)
