package js7.core

import akka.http.scaladsl.model.StatusCodes.ServiceUnavailable
import js7.base.problem.Problem
import js7.data.cluster.ClusterNodeId
import js7.data.event.EventId

/**
  * @author Joacim Zschimmer
  */
package object problems
{
  final case class JsonSeqFileClosedProblem(file: String) extends Problem.Coded {
    def arguments = Map("file" -> file)
  }

  final case class ReverseReleaseEventsProblem(requestedUntilEventId: EventId, currentUntilEventId: EventId) extends Problem.Coded {
    def arguments = Map(
      "requestedUntilEventId" -> requestedUntilEventId.toString,
      "currentUntilEventId" -> currentUntilEventId.toString)
  }

  final case object JobSchedulerIsShuttingDownProblem extends Problem.ArgumentlessCoded {
    override val httpStatusCode = ServiceUnavailable.intValue/*503*/
  }

  final case object ClusterNodeIsNotYetReadyProblem extends Problem.ArgumentlessCoded {
    override val httpStatusCode = ServiceUnavailable.intValue/*503*/
  }

  final case object ClusterNodeIsNotActiveProblem extends Problem.ArgumentlessCoded {
    override val httpStatusCode = ServiceUnavailable.intValue/*503*/
  }

  final case class MissingPassiveClusterNodeHeartbeatProblem(passiveId: ClusterNodeId) extends Problem.Coded {
    override def arguments = Map("passiveId" -> passiveId.toString)
  }

  object ClusterNodeIsNotBackupProblem extends Problem.ArgumentlessCoded

  object PrimaryMayNotBecomeBackupProblem extends Problem.ArgumentlessCoded

  final case object ClusterNodesAlreadyAppointed extends Problem.ArgumentlessCoded
}
