package js7.base.system

import js7.base.problem.Problem
import js7.base.test.OurTestSuite

final class LiveBeanMapViewTest extends OurTestSuite:

  "LiveBeanMapView" in:
    val exception = IllegalStateException()

    class Bean:
      def getInt = 7
      def getString = "STRING"
      def getError = throw exception

    val m = LiveBeanMapView(Bean())
    assert(m("int") == 7 && m("string") == "STRING")
    assert(m.toMap.apply("error") == Problem("IllegalStateException"))
    assert(m.toMap ==
      Map(
        "int" -> 7,
        "string" -> "STRING",
        "error" -> Problem("IllegalStateException")))

    val keys = Set("int", "string", "error")
    assert(m.keys.toSet == keys)
    assert(m.keysIterator.toSet == keys)
    assert(m.keySet == keys)
