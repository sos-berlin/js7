package js7.base.service

import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import scala.collection.immutable.Map.Map2

object Problems:

  final case class ServiceStoppedProblem(serviceName: String, extra: String = "")
  extends Problem.Coded:
    def arguments = Map2(
      "serviceName", serviceName,
      "suffix", extra.nonEmpty ?? s" • $extra")

  object ServiceStoppedProblem extends Problem.Coded.Companion
