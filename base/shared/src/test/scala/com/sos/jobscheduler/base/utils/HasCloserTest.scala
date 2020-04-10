package com.sos.jobscheduler.base.utils

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._

final class HasCloserTest extends AnyFreeSpec {

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
