package com.sos.jobscheduler.base.utils

import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class LazyTest extends FreeSpec
{
  "Lazy" in {
    var counter = 0
    val a = Lazy({ counter += 1; 7 })
    assert(!a.isDefined)
    assert(a.toOption == None)
    assert((a: Int) == 7)
    assert(counter == 1)
    assert(a.toOption == Some(7))
    assert((a: Option[Int]) == Some(7))
    assert(a.isDefined)
    assert(a() == 7)
    assert(a() == 7)
    assert(counter == 1)
  }
}
