package js7.base.problem

import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.test.OurTestSuite

final class ProblemsTest extends OurTestSuite:

  "UnknownKeyProblem unapply" in:
    def f() = "TYP"
    val problem: Problem = new/*avoid logging*/ UnknownKeyProblem(f(), "KEY")
    problem match
      case UnknownKeyProblem("TYP", "KEY") =>
      case _ => fail()
