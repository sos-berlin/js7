package js7.controller

import js7.base.problem.Problem

/**
  * @author Joacim Zschimmer
  */
package object problems
{
  final case object HistoricSnapshotServiceBusyProblem extends Problem.ArgumentlessCoded {
    override def httpStatusCode = 503  // Service Unavailable
  }

  final case object ControllerIsNotYetReadyProblem extends Problem.ArgumentlessCoded {
    override def httpStatusCode = 503  // Service Unavailable
  }
}
