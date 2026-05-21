package js7.data.event

import js7.base.time.Timestamp
import scala.concurrent.duration.FiniteDuration

/** Time context for use as a `Ctx` context for EventCalc and EventColl. */
final case class TimeCtx(timestamp: Timestamp, monotonic: FiniteDuration)
