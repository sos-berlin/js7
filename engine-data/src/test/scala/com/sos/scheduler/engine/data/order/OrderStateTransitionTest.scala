package com.sos.scheduler.engine.data.order

import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class OrderStateTransitionTest extends FreeSpec {

  "0 is success" in {
    OrderStateTransition.ofCppInternalValue(0) shouldEqual SuccessOrderStateTransition
  }

  "other is error" in {
    for (i ← -300 to 300; if i != 0)
      OrderStateTransition.ofCppInternalValue(i) shouldEqual ErrorOrderStateTransition(i)
  }

  "keep state" in {
    OrderStateTransition.ofCppInternalValue(Long.MaxValue) shouldEqual KeepOrderStateTransition
  }

  "invalid values" in {
    intercept[AssertionError] { OrderStateTransition.ofCppInternalValue(Int.MaxValue.toLong + 1) }
    intercept[AssertionError] { OrderStateTransition.ofCppInternalValue(Int.MinValue.toLong - 1) }
  }
}
