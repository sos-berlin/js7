package js7.controller

import js7.base.problem.Problem

package object problems
{
  case object ControllerIsNotReadyProblem extends Problem.ArgumentlessCoded {
    override val httpStatusCode = 503  // Service Unavailable
  }

  object ControllerIsShuttingDownProblem extends Problem.ArgumentlessCoded

  object ControllerIsSwitchingOverProblem extends Problem.ArgumentlessCoded
}
