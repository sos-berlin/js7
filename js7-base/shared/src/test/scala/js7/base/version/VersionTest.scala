package js7.base.version

import js7.base.problem.Problem
import js7.base.test.Test

final class VersionTest extends Test
{
  "Parser" in {
    assert(Version.checked("") == Left(Problem("Unrecognized version: ")))
    assert(Version.checked("1") == Left(Problem("Unrecognized version: 1")))
    assert(Version.checked("1.2.x") == Left(Problem("Unrecognized version: 1.2.x")))
    assert(Version.checked("1.2.9876543210") == Left(Problem("""NumberFormatException: For input string: "9876543210"""")))

    assertEqual(Version("0.0.1"), Version("0.0.1", 0, 0, 1))
    assertEqual(Version("1.2.3"), Version("1.2.3", 1 ,2, 3))
    assertEqual(Version("1.2.3-SNAPSHOT"), Version("1.2.3-SNAPSHOT", 1, 2, 3, List("SNAPSHOT")))
    assertEqual(Version("1.2.3-SNAPSHOT+1abc"), Version("1.2.3-SNAPSHOT+1abc", 1, 2, 3, List("SNAPSHOT"), List("1abc")))
    assertEqual(Version("1.2.3-alpha.20210324"), Version("1.2.3-alpha.20210324", 1 ,2, 3, List("alpha", "20210324")))
    assertEqual(Version("1.2.3+1abc"), Version("1.2.3+1abc", 1 ,2, 3, Nil, List("1abc")))

    def assertEqual(a: Version, b: Version) = {
      assert(a == b)
      assert(a.compare(b) == 0)
    }
  }

  "Compare lower case" in {
    // SNAPSHOT > 1.2.3-alpha but < 1.2.3
    assert(Version("0.0.1") > Version("0.0.0"))
    assert(Version("10.0.0") > Version("9.0.0"))
    assert(Version("1.2.4-SNAPSHOT") > Version("1.2.3"))
    assert(Version("1.2.3") > Version("1.2.3-SNAPSHOT"))
    assert(Version("1.2.3-alpha.20210324") > Version("1.2.3-SNAPSHOT"))
    assert(Version("1.2.3-beta") > Version("1.2.3-alpha.20210324"))
    assert(Version("1.2.3-beta.1") > Version("1.2.3-beta"))
    assert(Version("1.2.3-rc.1") > Version("1.2.3-beta.1"))
  }

  "Compare upper case" in {
    // SNAPSHOT > 1.2.3-alpha and > 1.2.3
    assert(Version("1.2.3") > Version("1.2.3-SNAPSHOT"))
    assert(Version("1.2.3-SNAPSHOT") > Version("1.2.3-ALPHA.1"))
    assert(Version("1.2.3-BETA.1") > Version("1.2.3-ALPHA.1"))
  }

  "Compare mixed upper and lower case" in {
    // Upper case letters are less than lowercase letters
    // 1.2.3-SNAPSHOT < 1.2.3-alpha < 1.2.3
    assert(Version("1.2.3") > Version("1.2.3-SNAPSHOT"))
    assert(Version("1.2.3-alpha") > Version("1.2.3-SNAPSHOT"))
    assert(Version("1.2.3-ALPHA") < Version("1.2.3-SNAPSHOT"))
    assert(Version("1.2.3") > Version("1.2.3-alpha"))
    assert(Version("1.2.3-BETA") < Version("1.2.3-alpha"))
  }

  "isMajorMinorEqual" in {
    assert(!Version("1.0.0").isMajorMinorEqual(Version("0.1.0")))
    assert(!Version("1.0.0").isMajorMinorEqual(Version("0.1.1")))
    assert(!Version("1.1.0").isMajorMinorEqual(Version("1.2.0")))
    assert(!Version("1.2.0").isMajorMinorEqual(Version("1.1.0")))

    assert(Version("1.0.1").isMajorMinorEqual(Version("1.0.0")))
    assert(Version("1.0.0").isMajorMinorEqual(Version("1.0.2")))
    assert(Version("1.0.1").isMajorMinorEqual(Version("1.0.2")))
  }
}
