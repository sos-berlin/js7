package com.sos.scheduler.engine.common.scalautil

import org.scalatest.FreeSpec
import org.scalatest.Matchers._

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
