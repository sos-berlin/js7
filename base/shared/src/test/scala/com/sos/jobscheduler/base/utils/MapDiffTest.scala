package js7.base.utils

import js7.base.circeutils.CirceUtils._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MapDiffTest extends AnyFreeSpec
{
  "applyTo" in {
    assert(MapDiff().applyTo(Map()) == Map())
    val variables = MapDiff(Map("a" -> "A", "b" -> "B", "c" -> "C"), Set("d")).applyTo(Map())
    assert(variables == Map("a" -> "A", "b" -> "B", "c" -> "C"))
    assert(MapDiff(Map("b" -> "BB"), Set("c")).applyTo(variables) ==
      Map("a" -> "A", "b" -> "BB"))
  }

  "diff" in {
    assert(MapDiff.diff(Map(), Map()) == MapDiff())
    assert(MapDiff.diff(Map("a" -> "A", "b" -> "B", "x" -> "X"), Map("a" -> "A", "b" -> "BBB", "c" -> "C")) ==
      MapDiff(Map("b" -> "BBB", "c" -> "C"), Set("x")))
  }

  "empty"in {
    val m = Map("a" -> "A")
    assert(MapDiff.empty.applyTo(m) == m)
  }

  "empty is singleton" in {
    assert(MapDiff(Map(), Set()) eq MapDiff.empty)
  }

  "empty MapDiff.apply retains same object" in {
    val m = Map("a" -> "A")
    pendingUntilFixed {  // TODO Scala 2.13 makes a copy, but result should be identical
      assert(MapDiff.empty.applyTo(m) eq m)
      assert(MapDiff().applyTo(m) eq m)
    }
  }

  "JSON" in {
    testJson(MapDiff[String, String](),
      json"""{
        "changed": {},
        "deleted": []
      }""")

    testJson(MapDiff(Map("a" -> "A", "b" -> "B"), Set("x", "y")),
      json"""{
        "changed": {
          "a": "A",
          "b": "B"
        },
        "deleted": [
          "x",
          "y"
        ]
      }""")
  }
}
