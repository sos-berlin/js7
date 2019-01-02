package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.base.problem.ProblemException
import com.sos.jobscheduler.data.event.{EventId, EventSeqTornProblem}

/**
  * @author Joacim Zschimmer
  */
final class TornException private[event](val after: EventId, val tornEventId: EventId)
extends ProblemException(EventSeqTornProblem(requestedAfter = after, tornEventId = tornEventId))
{
  override def getMessage = s"EventSeq is torn - requestedAfter=$after tornEventId=$tornEventId"
}
