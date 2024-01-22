package js7.core

import js7.base.problem.Problem
import js7.data.event.EventId

/**
  * @author Joacim Zschimmer
  */
package object problems:
  final case class ReverseReleaseEventsProblem(requestedUntilEventId: EventId, currentUntilEventId: EventId) extends Problem.Coded:
    def arguments = Map(
      "requestedUntilEventId" -> requestedUntilEventId.toString,
      "currentUntilEventId" -> currentUntilEventId.toString)
