package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class MapDiffTest extends FreeSpec {

  "applyTo" in {
    assert(MapDiff().applyTo(Map()) == Map())
    val variables = MapDiff(Map("a" → "A", "b" → "B", "c" → "C"), Set("d")).applyTo(Map())
    assert(variables == Map("a" → "A", "b" → "B", "c" → "C"))
    assert(MapDiff(Map("b" → "BB"), Set("c")).applyTo(variables) ==
      Map("a" → "A", "b" → "BB"))
  }

  "diff" in {
    assert(MapDiff.diff(Map(), Map()) == MapDiff())
    assert(MapDiff.diff(Map("a" → "A", "b" → "B", "x" → "X"), Map("a" → "A", "b" → "BBB", "c" → "C")) ==
      MapDiff(Map("b" → "BBB", "c" → "C"), Set("x")))
  }

  "empty"in {
    val m = Map("a" → "A")
    assert(MapDiff.empty.applyTo(m) == m)
  }

  "empty is singleton" in {
    assert(MapDiff(Map(), Set()) eq MapDiff.empty)
  }

  "empty MapDiff.apply retains same object" in {
    val m = Map("a" → "A")
    assert(MapDiff.empty.applyTo(m) eq m)
    assert(MapDiff().applyTo(m) eq m)
  }

  "JSON" in {
    testJson(MapDiff[String, String](),
      """{
        "changed": {},
        "deleted": []
      }""")

    testJson(MapDiff(Map("a" → "A", "b" → "B"), Set("x", "y")),
      """{
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
