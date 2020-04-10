package com.sos.jobscheduler.core.workflow.instructions

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.core.workflow.instructions.AwaitOrderExecutorTest._
import com.sos.jobscheduler.data.event.<-:
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAwaiting, OrderOffered}
import com.sos.jobscheduler.data.order.{Order, OrderId}
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, Offer}
import com.sos.jobscheduler.data.workflow.position.WorkflowPosition
import com.sos.jobscheduler.data.workflow.{WorkflowId, WorkflowPath}
import scala.concurrent.duration._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AwaitOrderExecutorTest extends AnyFreeSpec {

  "test" in {
    val context = new OrderContext {
      def idToOrder = Map(awaitingOrder.id -> awaitingOrder, offeredOrder.id -> offeredOrder)
      def childOrderEnded(order: Order[Order.State]) = throw new NotImplementedError
      def instruction(position: WorkflowPosition) = throw new NotImplementedError
      def idToWorkflow(id: WorkflowId) = throw new NotImplementedError
    }
    assert(InstructionExecutor.toEvent(AwaitOrder(offeredOrder.id), awaitingOrder, context) ==
      Right(Some(awaitingOrder.id <-: OrderAwaiting(offeredOrder.id))))

    val offerResult = InstructionExecutor.toEvent(Offer(OfferingOrderId, 60.seconds), offeredOrder, context)
    val Right(Some(OfferingOrderId <-: OrderOffered(OfferedOrderId, until))) = offerResult
    assert(until >= Timestamp.now + 50.seconds && until <= Timestamp.now + 70.seconds)
  }
}

object AwaitOrderExecutorTest {
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") ~ "VERSION"
  val OfferingOrderId = OrderId("OFFERED")
  val offeringOrder = Order(OfferingOrderId, TestWorkflowId, Order.Ready)
  val OfferedOrderId = OrderId("OFFERED")
  val offeredOrder = Order(OfferedOrderId, TestWorkflowId, Order.Ready)
  val awaitingOrderId = OrderId("AWAITING")
  val awaitingOrder = Order(awaitingOrderId, TestWorkflowId, Order.Ready)
}
