package js7.data.execution.workflow.instructions

import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax._
import js7.data.event.<-:
import js7.data.execution.workflow.context.StateView
import js7.data.execution.workflow.instructions.AwaitOrderExecutorTest._
import js7.data.order.OrderEvent.{OrderAwaiting, OrderOffered}
import js7.data.order.{Order, OrderId}
import js7.data.workflow.instructions.{AwaitOrder, Offer}
import js7.data.workflow.{WorkflowId, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class AwaitOrderExecutorTest extends AnyFreeSpec {

  "test" in {
    val stateView = new StateView {
      val idToOrder = Map(awaitingOrder.id -> awaitingOrder, offeredOrder.id -> offeredOrder).checked
      def childOrderEnded(order: Order[Order.State]) = throw new NotImplementedError
      def idToWorkflow(id: WorkflowId) = throw new NotImplementedError
      val pathToLockState = _ => Left(Problem("pathToLockState is not implemented"))
    }
    assert(InstructionExecutor.toEvents(AwaitOrder(offeredOrder.id), awaitingOrder, stateView) ==
      Right(Seq(awaitingOrder.id <-: OrderAwaiting(offeredOrder.id))))

    val offerResult = InstructionExecutor.toEvents(Offer(OfferingOrderId, 60.s), offeredOrder, stateView)
    val Right(Seq(OfferingOrderId <-: OrderOffered(OfferedOrderId, until))) = offerResult
    assert(until >= Timestamp.now + 50.s && until <= Timestamp.now + 70.s)
  }
}

object AwaitOrderExecutorTest {
  private val TestWorkflowId = WorkflowPath("WORKFLOW") ~ "VERSION"
  val OfferingOrderId = OrderId("OFFERED")
  val offeringOrder = Order(OfferingOrderId, TestWorkflowId, Order.Ready)
  val OfferedOrderId = OrderId("OFFERED")
  val offeredOrder = Order(OfferedOrderId, TestWorkflowId, Order.Ready)
  val awaitingOrderId = OrderId("AWAITING")
  val awaitingOrder = Order(awaitingOrderId, TestWorkflowId, Order.Ready)
}
