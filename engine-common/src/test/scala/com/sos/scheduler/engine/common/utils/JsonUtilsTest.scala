package com.sos.scheduler.engine.common.utils

import com.sos.scheduler.engine.common.utils.JsonUtils.jsonQuote
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
final class JsonUtilsTest extends FreeSpec {

  "jsonQuote" in {
    assert(jsonQuote("abc") == """"abc"""")
    assert(jsonQuote("""ab\c""") == """"ab\\c"""")
    assert(jsonQuote("""ab"c""") == """"ab\"c"""")
    assert(jsonQuote("ab\nc") == "\"ab\\u000ac\"")
    assert(jsonQuote(7) == """"7"""")
  }
}
