package com.sos.scheduler.engine.common.scalautil

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
final class HasCloserTest extends FreeSpec {

  "onClose" in {
    class A extends HasCloser {
      var closed = false
      onClose { closed = true }
    }
    val a = new A
    a.closed shouldBe false
    a.close()
    a.closed shouldBe true
  }
}
