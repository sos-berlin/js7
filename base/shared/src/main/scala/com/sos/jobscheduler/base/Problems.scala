package com.sos.jobscheduler.base

import com.sos.jobscheduler.base.problem.Problem

object Problems
{
  case object TamperedWithSignedMessageProblem extends Problem.ArgumentlessCoded

  case object MessageSignedByUnknownProblem extends Problem.ArgumentlessCoded
}
