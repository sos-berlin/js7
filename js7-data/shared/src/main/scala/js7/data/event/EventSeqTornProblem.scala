package js7.data.event

import js7.base.problem.Problem

/**
  * @author Joacim Zschimmer
  */
final case class EventSeqTornProblem(requestedAfter: EventId, tornEventId: EventId) extends Problem.Coded:
  def arguments = Map(
    "requestedAfter" -> EventId.toString(requestedAfter),
    "tornEventId" -> EventId.toString(tornEventId))

object EventSeqTornProblem extends Problem.Coded.Companion
