package com.sos.jobscheduler.core

import akka.http.scaladsl.model.StatusCodes.ServiceUnavailable
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.cluster.ClusterNodeId
import com.sos.jobscheduler.data.event.EventId

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
}
