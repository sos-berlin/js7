package js7.data.execution.workflow.instructions

import js7.base.time.Timestamp
import js7.data.event.<-:
import js7.data.execution.workflow.context.OrderContext
import js7.data.execution.workflow.instructions.AwaitOrderExecutorTest._
import js7.data.order.OrderEvent.{OrderAwaiting, OrderOffered}
import js7.data.order.{Order, OrderId}
import js7.data.workflow.instructions.{AwaitOrder, Offer}
import js7.data.workflow.position.WorkflowPosition
import js7.data.workflow.{WorkflowId, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

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
