package js7.data

import js7.base.problem.Problem
import js7.data.event.EventId
import scala.collection.immutable.Map.Map3

// TODO Move to object Problems for better IntelliJ support
/**
  * @author Joacim Zschimmer
  */
package object problems:

  final case class UnknownEventIdProblem(
    requestedEventId: EventId,
    tornEventId: EventId,
    lastAddedEventId: EventId)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map3(
      "requestedEventId", EventId.toString(requestedEventId),
      "tornEventId", EventId.toString(tornEventId),
      "lastAddedEventId", EventId.toString(lastAddedEventId))
  object UnknownEventIdProblem extends Problem.Coded.Companion

  case object UserIsNotEnabledToReleaseEventsProblem extends Problem.ArgumentlessCoded

  case object InvalidLoginProblem extends Problem.ArgumentlessCoded:
    override val httpStatusCode = 401/*Unauthorized*/

  object UnreachableOrderPositionProblem extends Problem.ArgumentlessCoded

  case object CannotResumeOrderProblem extends Problem.ArgumentlessCoded

  case object CannotSuspendOrderProblem extends Problem.ArgumentlessCoded
