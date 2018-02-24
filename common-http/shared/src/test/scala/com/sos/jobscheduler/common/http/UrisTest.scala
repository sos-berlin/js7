package com.sos.jobscheduler.common.http

import com.sos.jobscheduler.common.http.Uris._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class UrisTest extends FreeSpec {

  "encodePath" in {
    assert(encodePath("") == "")
    assert(encodePath("a") == "a")
    assert(encodePath("a", "b/c", "d") == "a/b%2Fc/d")
  }

  "encodeSegment" in {
    assert(encodeSegment("") == "")
    assert(encodeSegment("abc") == "abc")
    assert(encodeSegment("/a bcå") == "%2Fa%20bc%C3%A5")
  }

  "encodeQuery" in {
    assert(encodeQuery(Nil) == "")
    assert(encodeQuery(List("k" → "v", "?=" → "/&")) == "?k=v&?%3D=/%26")
  }
}
