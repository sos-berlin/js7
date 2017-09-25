package com.sos.jobscheduler.base.utils

import org.scalatest.FreeSpec
import spray.json.DefaultJsonProtocol._
import spray.json._

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

  "empty MapDiff.apply retains same object" in {
    val m = Map("a" → "A")
    assert(MapDiff.empty.applyTo(m) eq m)
    assert(MapDiff().applyTo(m) eq m)
  }

  "JSON" in {
    check(MapDiff[String, String](),
      """{
        "addedOrUpdated": {},
        "removed": []
      }""")

    check(MapDiff(Map("a" → "A", "b" → "B"), Set("x", "y")),
      """{
        "addedOrUpdated": {
          "a": "A",
          "b": "B"
        },
        "removed": [
          "x",
          "y"
        ]
      }""")

    def check[K: JsonFormat, V: JsonFormat](o: MapDiff[K, V], json: String): Unit = {
      assert(o.toJson == json.parseJson)
      assert(json.parseJson.convertTo[MapDiff[K, V]] == o)
    }
  }
}
