package js7.data.item

import js7.base.test.OurTestSuite

/**
  * @author Joacim Zschimmer
  */
final class VersionedItemTest extends OurTestSuite:
  "Companion" in:
    assert(AItem.typeName == "AItem")
    assert(AItem.toString == "AItem")
    assert(AItem.Path == APath)
