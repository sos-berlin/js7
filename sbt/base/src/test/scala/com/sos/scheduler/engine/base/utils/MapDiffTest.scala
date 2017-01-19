package com.sos.scheduler.engine.base.utils

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class MapDiffTest extends FreeSpec {

  "applyTo" in {
    assert(MapDiff(Map(), Set()).applyTo(Map()) == Map())
    val variables = MapDiff(Map("a" → "A", "b" → "B", "c" → "C"), Set("d")).applyTo(Map())
    assert(variables == Map("a" → "A", "b" → "B", "c" → "C"))
    assert(MapDiff(Map("b" → "BB"), Set("c")).applyTo(variables) ==
      Map("a" → "A", "b" → "BB"))
  }

  "diff" in {
    assert(MapDiff.diff(Map(), Map()) == MapDiff(Map(), Set()))
    assert(MapDiff.diff(Map("a" → "A", "b" → "B", "x" → "X"), Map("a" → "A", "b" → "BBB", "c" → "C")) ==
      MapDiff(Map("b" → "BBB", "c" → "C"), Set("x")))
  }

  "JSON" in {
    check(MapDiff[String, String](Map(), Set()),
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
