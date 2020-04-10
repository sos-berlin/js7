package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.generic.GenericString.EmptyStringProblem
import com.sos.jobscheduler.base.problem.ProblemException
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import io.circe.syntax.EncoderOps
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class VersionIdTest extends AnyFreeSpec
{
  "JSON" in {
    testJson(VersionId("X"), json""" "X" """)
    intercept[IllegalArgumentException] {
      VersionId.Anonymous.asJson
    }
  }

  "checked" in {
    assert(VersionId.checked("1") == Right(VersionId("1")))
    assert(VersionId.checked("") == Left(EmptyStringProblem("VersionId")))
    intercept[ProblemException] {
      VersionId("")
    }
  }
}
