package js7.data

import js7.base.problem.Problem
import js7.data.event.EventId

// TODO Move to object Problems for better IntelliJ support
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
