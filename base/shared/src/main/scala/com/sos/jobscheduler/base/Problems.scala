package js7.base

import js7.base.problem.Problem

object Problems
{
  case object TamperedWithSignedMessageProblem extends Problem.ArgumentlessCoded

  case object MessageSignedByUnknownProblem extends Problem.ArgumentlessCoded
}
