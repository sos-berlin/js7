package com.sos.scheduler.engine.base.utils

import com.sos.scheduler.engine.base.utils.ScalazStyle._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ScalazStyleTest extends FreeSpec {

  "Boolean.option" in {
    assert(true.option(7) == Some(7))
    assert(false.option(7) == None)
  }
}
