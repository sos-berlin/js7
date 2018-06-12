package com.sos.jobscheduler.base.auth

import com.sos.jobscheduler.base.problem.ProblemException
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class UserIdTest extends FreeSpec {

  "Invalid UserId" in {
    intercept[ProblemException] { UserId("") }
    intercept[ProblemException] { UserId("a?") }
    intercept[ProblemException] { UserId("a?b") }
    intercept[ProblemException] { UserId("a/b") }
    intercept[ProblemException] { UserId("/root") }
    intercept[ProblemException] { UserId(".") }
    intercept[ProblemException] { UserId("..") }
    intercept[ProblemException] { UserId(".hidden") }
  }

  "Valid UserId" in {
    UserId("a")
    UserId("å")
    UserId("0")
    UserId("ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÅÜabcdefghijklmnopqrstuvwxyzäöåüß0123456789.-_")
    UserId("a.")
  }
}
