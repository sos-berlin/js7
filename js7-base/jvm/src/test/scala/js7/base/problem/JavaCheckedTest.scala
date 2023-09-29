package js7.base.problem

import js7.base.problem.JavaCheckedTester.*
import js7.base.test.OurTestSuite

/**
  * @author Joacim Zschimmer
  */
final class JavaCheckedTest extends OurTestSuite:

  "isValid" in testIsValid
  "isInvalid" in testIsInvalid
  "toOptional" in testToOptional
  "problem" in testProblem
  "get" in testGet
