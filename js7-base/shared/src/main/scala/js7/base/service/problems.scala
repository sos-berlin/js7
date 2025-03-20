package js7.base.service

import js7.base.problem.Problem
import scala.collection.immutable.Map.Map1

final case class StoppedProblem(serviceName: String) extends Problem.Coded:
  def arguments = Map1("serviceName", serviceName)
