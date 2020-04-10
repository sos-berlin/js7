package com.sos.jobscheduler.base.problem

import com.sos.jobscheduler.base.problem.JavaCheckedTester._
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
