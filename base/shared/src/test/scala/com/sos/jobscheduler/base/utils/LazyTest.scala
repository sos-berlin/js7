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
    for (_ <- a) fail()
    //assert(a.map(_ * 3 ) == None)
    assert(a.toOption == None)
    assert((a: Int) == 7)
    assert(counter == 1)
    assert(a.toOption == Some(7))
    assert(a.isDefined)
    var foreachCalled = 0
    for (x <- a) foreachCalled += x
    assert(foreachCalled == 7)
    //assert(a.map(_ * 3 ) == Some(21))
    assert(a() == 7)
    assert(a() == 7)
    assert(counter == 1)
  }
}
