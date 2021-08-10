package js7.controller

import js7.base.problem.Problem

package object problems
{
  case object ControllerIsNotYetReadyProblem extends Problem.ArgumentlessCoded {
    override val httpStatusCode = 503  // Service Unavailable
  }
}
