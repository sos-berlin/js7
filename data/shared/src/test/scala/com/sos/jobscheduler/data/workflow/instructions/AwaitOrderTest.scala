package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.data.event.<-:
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAwaiting, OrderOffered}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.AwaitOrderTest._
import com.sos.jobscheduler.data.workflow.{OrderContext, WorkflowPath, WorkflowPosition}
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class AwaitOrderTest extends FreeSpec {

  "test" in {
    val context = new OrderContext {
      def idToOrder = Map(awaitingOrder.id → awaitingOrder, offeredOrder.id → offeredOrder)
      def childOrderEnded(order: Order[Order.State]) = throw new NotImplementedError()
      def instruction(position: WorkflowPosition) = throw new NotImplementedError
    }
    assert(AwaitOrder(offeredOrder.id).toEvent(awaitingOrder, context) ==
      Some(awaitingOrder.id <-: OrderAwaiting(offeredOrder.id)))

    val offerResult = Offer(OfferingOrderId, 60.seconds).toEvent(offeredOrder, context)
    val Some(OfferingOrderId <-: OrderOffered(OfferedOrderId, until)) = offerResult
    assert(until >= now + 50.seconds && until <= now + 70.seconds)
  }
}

object AwaitOrderTest {
  val OfferingOrderId = OrderId("OFFERED")
  val offeringOrder = Order(OfferingOrderId, WorkflowPath("/WORKFLOW"), Order.Ready)
  val OfferedOrderId = OrderId("OFFERED")
  val offeredOrder = Order(OfferedOrderId, WorkflowPath("/WORKFLOW"), Order.Ready)
  val awaitingOrderId = OrderId("AWAITING")
  val awaitingOrder = Order(awaitingOrderId, WorkflowPath("/WORKFLOW"), Order.Ready)
}
