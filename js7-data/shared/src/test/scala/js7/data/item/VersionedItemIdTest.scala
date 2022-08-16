package js7.data.item

import js7.base.circeutils.CirceUtils.*
import js7.base.test.OurTestSuite
import js7.tester.CirceJsonTester.testJson

/**
  * @author Joacim Zschimmer
  */
final class VersionedItemIdTest extends OurTestSuite {

  "JSON" in {
    testJson[VersionedItemId[APath]](
      VersionedItemId(APath("PATH"), VersionId("VERSION")),
      json"""{
        "path": "PATH",
        "versionId": "VERSION"
      }""")
  }

  "isAnonymous" in {
    assert(APath.NoId.isAnonymous)
    assert(!VersionedItemId(APath("PATH"), VersionId.Anonymous).isAnonymous)
    assert(!VersionedItemId(APath.Anonymous, VersionId("1")).isAnonymous)
    assert(!VersionedItemId(APath("PATH"), VersionId("1")).isAnonymous)
  }

  "toString" in {
    assert(((APath("PATH")) ~ "1").toString == "A:PATH~1")
  }

  "toTypedString" in {
    assert(((APath("PATH")) ~ "1").toTypedString == "A:PATH~1")
  }

  "pretty" in {
    assert(((APath("PATH")) ~ "1").pretty == "A:PATH~1")
  }

  "toSimpleString" in {
    assert(((APath("PATH")) ~ "1").toSimpleString == "PATH~1")
  }
}
