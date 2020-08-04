package js7.data.item

import js7.base.circeutils.CirceUtils._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ItemIdTest extends AnyFreeSpec {

  "JSON" in {
    testJson[ItemId[APath]](
      ItemId(APath("/PATH"), VersionId("VERSION")),
      json"""{
        "path": "/PATH",
        "versionId": "VERSION"
      }""")
  }

  "isAnonymous" in {
    assert(APath.NoId.isAnonymous)
    assert(!ItemId(APath("/PATH"), VersionId.Anonymous).isAnonymous)
    assert(!ItemId(APath.Anonymous, VersionId("1")).isAnonymous)
    assert(!ItemId(APath("/PATH"), VersionId("1")).isAnonymous)
  }
}
