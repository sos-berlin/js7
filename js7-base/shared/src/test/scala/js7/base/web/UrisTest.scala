package js7.base.web

import js7.base.test.OurTestSuite
import js7.base.web.Uris.{encodePath, encodeQuery, encodeSegment}

/**
  * @author Joacim Zschimmer
  */
final class UrisTest extends OurTestSuite:

  "encodePath" in:
    assert(encodePath("") == "")
    assert(encodePath("a") == "a")
    assert(encodePath("a", "b/c", "d") == "a/b%2Fc/d")

  "encodeSegment" in:
    assert(encodeSegment("") == "")
    assert(encodeSegment("abc") == "abc")
    assert(encodeSegment("/a bcå") == "%2Fa%20bc%C3%A5")

  "encodeQuery" in:
    assert(encodeQuery(Nil) == "")
    assert(encodeQuery(List("k" -> "v", "?=" -> "/&")) == "?k=v&?%3D=/%26")
