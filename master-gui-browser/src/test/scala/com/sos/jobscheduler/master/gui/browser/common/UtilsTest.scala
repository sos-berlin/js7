package com.sos.jobscheduler.master.gui.browser.common

import com.sos.jobscheduler.master.gui.browser.common.Utils._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class UtilsTest extends FreeSpec {

  "toUriQueryString" in {
    assert(toUriQueryString(Nil) == "")
    assert(toUriQueryString(("a" → "A") :: Nil) == "a=A")
    assert(toUriQueryString(("a" → "Ä") :: ("b=" → "B&") :: Nil) == "a=%C3%84&b%3D=B%26")
  }

  "stringToHtml" in {
    assert(stringToHtml("<script>hallo&amp;</script>") == "&lt;script>hallo&amp;amp;&lt;/script")
    assert(stringToHtml("<![CDATA[ bla ]]>") == "&lt;![CDATA[ bla ]]&g;t")
  }
}
