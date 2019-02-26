package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.problem.Problem

/**
  * @author Joacim Zschimmer
  */
final case class EventSeqTornProblem(requestedAfter: EventId, tornEventId: EventId) extends Problem.Coded {
  def arguments = Map(
    "requestedAfter" -> requestedAfter.toString,
    "tornEventId" -> tornEventId.toString)
}
