package js7.base.utils

import js7.base.test.OurTestSuite
import scala.collection.MapView

final class KeyRenamingMapViewTest extends OurTestSuite:

  "test" in:
    val original = MapView("one" -> 1, "two" -> 2)
    val renamed = KeyRenamingMapView(Seq("one" -> "eins", "two" -> "zwei"))(original)
    assert(renamed.keySet == Set("eins", "zwei"))
    assert(renamed.keys.toSet == Set("eins", "zwei"))
    assert(renamed.keysIterator.toSet == Set("eins", "zwei"))
    assert(renamed.values.toSet == Set(1, 2))
    assert(renamed("eins") == 1)
    assert(renamed("zwei") == 2)
    assert(renamed.get("eins") == Some(1))
    assert(renamed.get("zwei") == Some(2))
    assert(renamed.get("one") == None)
