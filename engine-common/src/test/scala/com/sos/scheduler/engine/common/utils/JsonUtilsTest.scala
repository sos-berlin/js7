package com.sos.scheduler.engine.common.utils

import com.sos.scheduler.engine.common.utils.JsonUtils.jsonQuote
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class JsonUtilsTest extends FreeSpec {

  "jsonQuote" in {
    assert(jsonQuote("abc") == """"abc"""")
    assert(jsonQuote("""ab\c""") == """"ab\\c"""")
    assert(jsonQuote("""ab"c""") == """"ab\"c"""")
    assert(jsonQuote("ab\nc") == "\"ab\\u000ac\"")
    assert(jsonQuote(7) == """"7"""")
  }
}
