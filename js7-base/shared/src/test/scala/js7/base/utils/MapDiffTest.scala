package js7.base.utils

import js7.base.circeutils.CirceUtils._
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class MapDiffTest extends AnyFreeSpec
{
  "applyTo" in {
    assert(MapDiff.empty.applyTo(Map.empty) == Map.empty)
    val a = MapDiff(Map("a" -> "A", "b" -> "B"), Map( "c" -> "C"), Set("d")).applyTo(Map.empty)
    assert(a == Map("a" -> "A", "b" -> "B", "c" -> "C"))
  }

  "diff" in {
    assert(MapDiff.diff(Map.empty, Map.empty) eq MapDiff.empty)
    assert(MapDiff.diff(
      Map("a" -> "A", "b" -> "B", "x" -> "X"),
      Map("a" -> "A", "b" -> "BBB", "c" -> "C")
    ) == MapDiff(Map("c" -> "C"), Map("b" -> "BBB"), Set("x")))
  }

  "empty"in {
    val m = Map("a" -> "A")
    assert(MapDiff.empty.applyTo(m) eq m)
  }

  "empty is singleton" in {
    assert(MapDiff(Map.empty, Map.empty, Set.empty) eq MapDiff.empty)
  }

  "empty MapDiff.apply retains same object" in {
    val m = Map("a" -> "A")
    assert(MapDiff.empty.applyTo(m) eq m)
  }

  "JSON" in {
    testJson(MapDiff.empty[String, String],
      json"""{
        "added": {},
        "updated": {},
        "deleted": []
      }""")

    testJson(MapDiff(Map("a" -> "A", "b" -> "B"), Map("u" -> "U"), Set("d", "D")),
      json"""{
        "added": {
          "a": "A",
          "b": "B"
        },
        "updated": {
          "u": "U"
        },
        "deleted": ["d", "D"]
      }""")
  }
}
