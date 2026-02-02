package js7.data.event

import js7.base.time.Timestamp

/** Time context for use as a `Ctx` context for EventCalc and EventColl. */
type TimeCtx = Timestamp


object TimeCtx:

  inline def apply(now: Timestamp): TimeCtx =
    now
