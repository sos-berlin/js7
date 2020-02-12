package com.sos.jobscheduler.master

import com.sos.jobscheduler.base.problem.Problem

/**
  * @author Joacim Zschimmer
  */
package object problems
{
  final case object FatEventServiceBusyProblem extends Problem.ArgumentlessCoded

  final case object MasterIsNotYetReadyProblem extends Problem.ArgumentlessCoded {
    override def httpStatusCode = 503  // Service Unavailable
  }
}
