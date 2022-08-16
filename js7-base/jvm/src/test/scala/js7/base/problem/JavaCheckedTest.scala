package js7.base.problem

import js7.base.problem.JavaCheckedTester.*
import js7.base.test.Test

/**
  * @author Joacim Zschimmer
  */
final class JavaCheckedTest extends Test {

  "isValid" in testIsValid
  "isInvalid" in testIsInvalid
  "toOptional" in testToOptional
  "problem" in testProblem
  "get" in testGet
}
