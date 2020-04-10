package com.sos.jobscheduler.data

import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.event.EventId

/**
  * @author Joacim Zschimmer
  */
package object problems
{
  final case class UnknownEventIdProblem(requiredEventId: EventId) extends Problem.Coded {
    def arguments = Map("requiredEventId" -> EventId.toString(requiredEventId))
  }
  object UnknownEventIdProblem extends Problem.Coded.Companion

  case object UserIsNotEnabledToReleaseEventsProblem extends Problem.ArgumentlessCoded
}
