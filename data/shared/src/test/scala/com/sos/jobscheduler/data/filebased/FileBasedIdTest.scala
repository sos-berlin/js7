package js7.data.filebased

import js7.base.circeutils.CirceUtils._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FileBasedIdTest extends AnyFreeSpec {

  "JSON" in {
    testJson[FileBasedId[APath]](
      FileBasedId(APath("/PATH"), VersionId("VERSION")),
      json"""{
        "path": "/PATH",
        "versionId": "VERSION"
      }""")
  }

  "isAnonymous" in {
    assert(APath.NoId.isAnonymous)
    assert(!FileBasedId(APath("/PATH"), VersionId.Anonymous).isAnonymous)
    assert(!FileBasedId(APath.Anonymous, VersionId("1")).isAnonymous)
    assert(!FileBasedId(APath("/PATH"), VersionId("1")).isAnonymous)
  }
}
