package com.sos.jobscheduler.data.workflow.transition

import com.sos.jobscheduler.data.order.{LeanOrder, OrderEvent, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.test.TestSetting._
import com.sos.jobscheduler.data.workflow.transition.TransitionTest._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TransitionTest extends FreeSpec {

  "switch" in {
    val nodeToOrder = Map(A.id → TestOrder.toLean, B.id → OtherOrder)
    val t = Transition(from = A, to = B, TestTransition)
    assert(t.canSwitch(nodeToOrder))
    assert(t.switch(nodeToOrder) == Some(OrderEvent.OrderTransitioned(B.id)))
  }
}

object TransitionTest {
  private object TestTransition extends OneToOneTransition {
    def result(order: LeanOrder) = 0
      //OrderEvent.OrderTransitioned.Result(order.id, MapDiff(Map("SWITCHED" → "1")), order.outcome)
  }

  private val OtherOrder = LeanOrder(OrderId("OTHER"), Payload(Map()))
}
