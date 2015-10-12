package com.sos.scheduler.engine.common.scalautil

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers._

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class SetOnceTest extends FreeSpec {

  "SetOnce" in {
    val a = new SetOnce[Int]
    assert(a.isEmpty)
    assert(!a.nonEmpty)
    assert(!a.isDefined)
    intercept[IllegalStateException] { a() }
    assert(a.get == None)
    assert((a getOrElse -1) == -1)
    a := 0
    assert(!a.isEmpty)
    assert(a.nonEmpty)
    assert(a.isDefined)
    assert(a() == 0)
    assert(a.get == Some(0))
    assert((a getOrElse -1) == -0)
    intercept[IllegalStateException] { a := 0 } .getMessage should include ("SetOnce[java.lang.Integer]")
  }
}
