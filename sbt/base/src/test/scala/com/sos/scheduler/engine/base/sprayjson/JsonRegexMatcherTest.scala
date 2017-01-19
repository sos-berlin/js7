package com.sos.scheduler.engine.base.sprayjson

import com.sos.scheduler.engine.base.sprayjson.JsonRegexMatcher._
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
final class JsonRegexMatcherTest extends FreeSpec {
  "testRegexJson" in {
    val json = """{"a": 1, "b": "BB"}"""
    intercept[RuntimeException] { testRegexJson(json, Map("a" → 1)) }
    intercept[RuntimeException] { testRegexJson(json, Map("a" → 1, "b" → 2, "c" → 3)) }
    intercept[RuntimeException] { testRegexJson("""{"a": "XX"}""", Map("a" → "A+".r)) }
    intercept[RuntimeException] { testRegexJson(json, Map("a" → AnyInt, "b" → "B".r)) }
    testRegexJson(json, Map("a" → 1, "b" → "BB"))
    testRegexJson(json, Map("a" → AnyInt, "b" → """B+""".r))
  }
}
