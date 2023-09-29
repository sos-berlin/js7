package js7.base

import js7.base.problem.Problem

object Problems:
  case object TamperedWithSignedMessageProblem extends Problem.ArgumentlessCoded

  final case class UnknownSignatureTypeProblem(typeName: String) extends Problem.Coded:
    def arguments = Map("typeName" -> typeName)

  case object MessageSignedByUnknownProblem extends Problem.ArgumentlessCoded
