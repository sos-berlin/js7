package js7.common.event

import js7.base.problem.ProblemException
import js7.data.event.{EventId, EventSeqTornProblem}

/**
  * @author Joacim Zschimmer
  */
final class TornException private[event](val after: EventId, val tornEventId: EventId)
extends ProblemException(EventSeqTornProblem(requestedAfter = after, tornEventId = tornEventId))
{
  override def getMessage = s"EventSeq is torn - requestedAfter=$after tornEventId=$tornEventId"
}
