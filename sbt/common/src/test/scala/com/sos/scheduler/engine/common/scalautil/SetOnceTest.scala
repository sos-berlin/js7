package com.sos.scheduler.engine.common.scalautil

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

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
    assert((for (i ← a) yield (i: Int) + 3) == Some(3))
    var r = 7
    for (i ← a) r = a()
    assert(r == 0)
  }

  "getOrUpdate" in {
    val a = new SetOnce[Int]
    assert((a getOrUpdate 1) == 1)
    assert((a getOrUpdate 2) == 1)
    assert((a getOrUpdate sys.error("")) == 1)
  }

  "SetOnce with Implicit" in {
    val a = new SetOnce[Int] with SetOnce.Implicit
    intercept[IllegalStateException] { a: Int }
    a := 0
    assert((a: Int) == 0)
  }
}
