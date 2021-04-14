package js7.data.item

import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class VersionedItemTest extends AnyFreeSpec
{
  "Companion" in {
    assert(AItem.typeName == "AItem")
    assert(AItem.toString == "AItem")
    assert(AItem.Path == APath)
  }
}
