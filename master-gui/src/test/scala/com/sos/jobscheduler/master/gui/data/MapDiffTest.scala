package com.sos.jobscheduler.master.gui.data

import io.circe.parser.parse
import io.circe.syntax._
import org.scalatest.FreeSpec

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

    def check(o: MapDiff[String, String], json: String): Unit = {
      assert(o.asJson == parse(json).toTry.get)
      assert(parse(json).toTry.get.as[MapDiff[String, String]] == Right(o))
    }
  }
}
