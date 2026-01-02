package js7.base

import js7.base.problem.Problem
import scala.collection.immutable.Map.Map1

object Problems:
  case object TamperedWithSignedMessageProblem extends Problem.ArgumentlessCoded

  final case class UnknownSignatureTypeProblem(typeName: String) extends Problem.Coded:
    def arguments: Map[String, String] = Map(
      "typeName" -> typeName)

  case object MessageSignedByUnknownProblem extends Problem.ArgumentlessCoded

  final case class ConcurrentAccessProblem(operation: String) extends Problem.Coded:
    def arguments = Map1("operation", operation)

  object ConcurrentAccessProblem extends Problem.Coded.Companion