package js7.data.filebased

import js7.base.circeutils.CirceUtils._
import js7.base.generic.GenericString.EmptyStringProblem
import js7.base.problem.ProblemException
import js7.tester.CirceJsonTester.testJson
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
