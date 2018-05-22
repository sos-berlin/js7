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
}
