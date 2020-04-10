package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.utils.SideEffect._
import org.scalatest.matchers
import org.scalatest.matchers.should.Matchers._
import org.scalatest.freespec.AnyFreeSpec

final class SideEffectTest extends AnyFreeSpec {

  "sideEffect" in {
    val a = A(1) sideEffect { _.x = 2 }
    a.x shouldEqual 2
  }

  private case class A(var x: Int)
}
