package js7.base.problem

import js7.base.problem.JavaCheckedTester.*
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JavaCheckedTest extends AnyFreeSpec {

  "isValid" in testIsValid
  "isInvalid" in testIsInvalid
  "toOptional" in testToOptional
  "problem" in testProblem
  "get" in testGet
}
