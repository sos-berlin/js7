package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.NodeKey
import com.sos.jobscheduler.data.workflow.test.TestSetting._
import com.sos.jobscheduler.data.workflow.transition.Transition
import com.sos.jobscheduler.shared.workflow.ForwardTransitionTest._
import com.sos.jobscheduler.shared.workflow.Transitions.ExecutableTransition
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ForwardTransitionTest extends FreeSpec {

  "switch" in {
    val order = TestOrder.copy(state = Order.Processed)
    val nodeToOrder = Map(A.id → order, B.id → OtherOrder)
    assert(TestTransition.canSwitch(nodeToOrder))
    assert(TestTransition.switch(nodeToOrder) == Some(
      KeyedEvent(OrderEvent.OrderMoved(B.id))(order.id)))
  }
}

object ForwardTransitionTest {
  private val TestTransition = Transition(from = A, to = B)
  private val OtherOrder = Order(OrderId("OTHER"), NodeKey(TestWorkflow.path, B.id), Order.Processed)
}
