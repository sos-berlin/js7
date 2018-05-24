package com.sos.jobscheduler.base.problem

import com.sos.jobscheduler.base.problem.JavaCheckedTester._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class JavaCheckedTest extends FreeSpec {

  "isValid" in testIsValid
  "isInvalid" in testIsInvalid
  "toOptional" in testToOptional
  "problem" in testProblem
  "get" in testGet
}
