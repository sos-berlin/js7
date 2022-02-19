package js7.data.session

import js7.base.version.Version
import org.scalatest.freespec.AnyFreeSpec

final class HttpSessionApiTest extends AnyFreeSpec
{
  "checkNonMatchingVersion" - {
    // Check log output manually!
    val ourVersion = Version("2.2.2")

    def log(v: Version) =
      HttpSessionApi.logNonMatchingVersion(v, otherName = "OTHER", ourVersion = ourVersion)

    "valid" - {
      val validVersions = Seq("2.2.1", "2.2.2", "2.2.3", "2.2.3-SNAPSHOT+BUILD")
        .map(Version(_))
      for (v <- validVersions) log(v)
    }

    "invalid" in {
      val invalidVersions = Seq("2.1.0", "2.3.0")
        .map(Version(_))
      for (v <- invalidVersions) log(v)
    }
  }
}
