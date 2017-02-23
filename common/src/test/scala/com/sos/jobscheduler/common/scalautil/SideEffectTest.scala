package com.sos.scheduler.engine.common.scalautil

import com.sos.scheduler.engine.common.scalautil.SideEffect._
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

final class SideEffectTest extends FreeSpec {

  "sideEffect" in {
    val a = A(1) sideEffect { _.x = 2 }
    a.x shouldEqual 2
  }

  private case class A(var x: Int)
}
