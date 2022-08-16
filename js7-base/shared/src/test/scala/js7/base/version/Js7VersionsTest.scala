package js7.base.version

import js7.base.problem.Problem
import js7.base.test.Test

final class Js7VersionsTest extends Test
{
  "checkNonMatchingVersion" - {
    val ourVersion = Version("2.2.2")

    def check(v: Version) = {
      Js7Versions.checkNonMatchingVersion(
        v,
        otherName = "OTHER",
        ourVersion = ourVersion)
    }

    "valid" - {
      val validVersions = Seq("2.2.1", "2.2.2", "2.2.3", "2.2.3-SNAPSHOT+BUILD")
        .map(Version(_))
      for (v <- validVersions) assert(check(v) == Right(()))
    }

    "invalid" in {
      val invalidVersions = Seq("2.1.0", "2.3.0")
        .map(Version(_))
      for (v <- invalidVersions) assert(check(v) == Left(Problem(
        s"OTHER version $v does not match our version 2.2.2")))
    }
  }
}
