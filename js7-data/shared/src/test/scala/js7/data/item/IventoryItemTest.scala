package js7.data.item

import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class IventoryItemTest extends AnyFreeSpec
{
  "Companion" in {
    assert(AItem.name == "AItem")
    assert(AItem.toString == "AItem")
    assert(AItem.typedPathCompanion == APath)
  }
}
