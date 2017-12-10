package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
import com.sos.jobscheduler.data.workflow.{NodeId, NodeKey}
import com.sos.jobscheduler.shared.workflow.Transitions._
import com.sos.jobscheduler.shared.workflow.TransitionsTest._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TransitionsTest extends FreeSpec {

  "joinableOrders" - {
    "all orders at Master" in {
      val aOrderId = OrderId("A")
      val a1Order = makeOrder(OrderId("A/🥕"), Cx.id, Order.Processed, parent = Some(aOrderId))
      val a2Order = makeOrder(OrderId("A/🍋"), Cy.id, Order.Processed, parent = Some(aOrderId))
      val aOrder  = makeOrder(aOrderId, A.id , Order.Forked(Vector(a1Order.id, a2Order.id)), parent = None)

      val allOrders = Set(aOrder, a1Order, a2Order)
      for (order ← allOrders) {
        assert(c.joinableOrders(order, List(a1Order, a2Order, aOrder).toKeyedMap(_.id)).toSet == allOrders)
      }
    }

    "one order at Agent" in {
      val aOrderId = OrderId("A")
      val a1Order = makeOrder(OrderId("A/🥕"), Cx.id, Order.Processed, parent = Some(aOrderId))
      val a2Order = makeOrder(OrderId("A/🍋"), Cy.id, Order.Processed, parent = Some(aOrderId))
        .copy(attachedTo = Some(Order.AttachedTo.Agent(AAgentPath)))
      val aOrder  = makeOrder(aOrderId, A.id , Order.Forked(Vector(a1Order.id, a2Order.id)), parent = None)

      val expected = Set(aOrder, a1Order)
      for (order ← Set(aOrder, a1Order)) withClue(s"${order.id}: ") {
        assert(c.joinableOrders(order, List(a1Order, a2Order, aOrder).toKeyedMap(_.id)).toSet == expected)
      }

      assert(c.joinableOrders(a2Order, List(a1Order, a2Order, aOrder).toKeyedMap(_.id)).isEmpty)
    }
  }
}

object TransitionsTest {
  private def makeOrder[S <: Order.State](orderId: OrderId, nodeId: NodeId, state: S, parent: Option[OrderId]) =
    Order(orderId, NodeKey(TestWorkflow.path, nodeId), state, parent = parent)
}
