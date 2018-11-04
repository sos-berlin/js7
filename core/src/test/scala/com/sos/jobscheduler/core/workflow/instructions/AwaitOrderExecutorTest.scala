package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.core.workflow.instructions.AwaitOrderExecutorTest._
import com.sos.jobscheduler.data.event.<-:
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAwaiting, OrderOffered}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, Offer}
import com.sos.jobscheduler.data.workflow.position.WorkflowPosition
import com.sos.jobscheduler.data.workflow.{OrderContext, WorkflowPath}
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class AwaitOrderExecutorTest extends FreeSpec {

  "test" in {
    val context = new OrderContext {
      def idToOrder = Map(awaitingOrder.id → awaitingOrder, offeredOrder.id → offeredOrder)
      def childOrderEnded(order: Order[Order.State]) = throw new NotImplementedError
      def instruction(position: WorkflowPosition) = throw new NotImplementedError
    }
    assert(InstructionExecutor.toEvent(AwaitOrder(offeredOrder.id), awaitingOrder, context) ==
      Some(awaitingOrder.id <-: OrderAwaiting(offeredOrder.id)))

    val offerResult = InstructionExecutor.toEvent(Offer(OfferingOrderId, 60.seconds), offeredOrder, context)
    val Some(OfferingOrderId <-: OrderOffered(OfferedOrderId, until)) = offerResult
    assert(until >= now + 50.seconds && until <= now + 70.seconds)
  }
}

object AwaitOrderExecutorTest {
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") % "VERSION"
  val OfferingOrderId = OrderId("OFFERED")
  val offeringOrder = Order(OfferingOrderId, TestWorkflowId, Order.Ready)
  val OfferedOrderId = OrderId("OFFERED")
  val offeredOrder = Order(OfferedOrderId, TestWorkflowId, Order.Ready)
  val awaitingOrderId = OrderId("AWAITING")
  val awaitingOrder = Order(awaitingOrderId, TestWorkflowId, Order.Ready)
}
