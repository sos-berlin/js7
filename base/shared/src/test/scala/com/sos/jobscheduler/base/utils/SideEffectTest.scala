package com.sos.jobscheduler.base.utils

import com.sos.jobscheduler.base.utils.SideEffect._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

final class SideEffectTest extends FreeSpec {

  "sideEffect" in {
    val a = A(1) sideEffect { _.x = 2 }
    a.x shouldEqual 2
  }

  private case class A(var x: Int)
}
