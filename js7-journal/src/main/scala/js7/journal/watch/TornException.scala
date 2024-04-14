package js7.journal.watch

import js7.base.problem.ProblemException
import js7.data.event.{EventId, EventSeqTornProblem}
import scala.util.control.NoStackTrace

/**
  * @author Joacim Zschimmer
  */
final class TornException private[watch](val after: EventId, val tornEventId: EventId)
extends ProblemException(EventSeqTornProblem(requestedAfter = after, tornEventId = tornEventId)),
  NoStackTrace
