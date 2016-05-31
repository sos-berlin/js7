package com.sos.scheduler.engine.common.utils

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class BeanAccessorTest extends FreeSpec {

  private class TestBean {
    var int = 1
    var string = "STRING"
    def getInt = int
    def setInt(o: Int): Unit = int = o
    def getString = string
    def setString(o: String): Unit = string = o
  }

  "test" in {
    val bean = new TestBean
    val a = BeanAccessor.forAny(bean)
    assert(a("int") == 1)
    assert(a("string") == "STRING")

    a("int") = Integer valueOf 2
    assert(bean.int == 2)
    assert(a("int") == 2)

    a("string") = "CHANGED"
    assert(bean.string == "CHANGED")
    assert(a("string") == "CHANGED")
  }
}
