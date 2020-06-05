package js7.data.execution.workflow.instructions

import js7.base.problem.Problem
import js7.base.time.Timestamp
import js7.data.execution.workflow.context.OrderContext
import js7.data.execution.workflow.instructions.RetryExecutorTest._
import js7.data.job.ReturnCode
import js7.data.order.OrderEvent.OrderRetrying
import js7.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import js7.data.workflow.instructions.{Gap, Retry, TryInstruction}
import js7.data.workflow.position.{Position, WorkflowPosition}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class RetryExecutorTest extends AnyFreeSpec
{
  "toEvent" in {
    assert(toEvent(Position(1)) == Left(Problem("Retry, but not in a catch-block")))
    assert(toEvent(tryPosition) == Left(Problem("Retry, but not in a catch-block")))
    assert(toEvent(tryPosition / "try+0" % 0) == Left(Problem("Retry, but not in a catch-block")))
  }

  "Find try Position and increment BranchId" in {
    assert(toEvent(tryPosition / "catch+0" % 0) == Right(Some(orderId <-: OrderRetrying(tryPosition / "try+1" % 0))))
    assert(toEvent(tryPosition / "catch+1" % 0) == Right(Some(orderId <-: OrderRetrying(tryPosition / "try+2" % 0))))
  }

  "Delay" in {
    val delays = 0.second :: 1.second :: 2.seconds :: 11.5.seconds :: Nil
    assert(toEvent(tryPosition / "catch+0" % 0, delays) == Right(Some(orderId <-: OrderRetrying(tryPosition / "try+1" % 0, None))))
    assert(toEvent(tryPosition / "catch+1" % 0, delays) == Right(Some(orderId <-: OrderRetrying(tryPosition / "try+2" % 0, Some(now + 1.second)))))
    assert(toEvent(tryPosition / "catch+2" % 0, delays) == Right(Some(orderId <-: OrderRetrying(tryPosition / "try+3" % 0, Some(now + 2.seconds)))))
    assert(toEvent(tryPosition / "catch+3" % 0, delays) == Right(Some(orderId <-: OrderRetrying(tryPosition / "try+4" % 0, Some(now + 12.seconds)))))
    assert(toEvent(tryPosition / "catch+4" % 0, delays) == Right(Some(orderId <-: OrderRetrying(tryPosition / "try+5" % 0, Some(now + 12.seconds)))))
  }
}

object RetryExecutorTest
{
  private val now = Timestamp("2019-03-08T12:00:00Z")
  private val orderId = OrderId("ORDER-A")
  private val tryPosition = Position(7)
  private val workflowId = WorkflowPath("/WORKFLOW") ~ "VERSION"
  private val tryInstruction = TryInstruction(Workflow.empty, Workflow.empty)

  private def toEvent(position: Position, delays: Seq[FiniteDuration] = Nil) = {
    val order = Order(orderId, workflowId /: position, Order.Ready,
      historicOutcomes = HistoricOutcome(Position(0), Outcome.Succeeded(ReturnCode(1))) :: Nil)
    val context = new OrderContext {
      def idToOrder = Map(order.id -> order)
      def childOrderEnded(order: Order[Order.State]) = throw new NotImplementedError
      def instruction(position: WorkflowPosition) =
        if (position == workflowId /: tryPosition) tryInstruction.copy(retryDelays = Some(delays.toVector))
        else Gap()
      def idToWorkflow(id: WorkflowId) = throw new NotImplementedError
    }
    new RetryExecutor(() => now).toEvent(context, order, Retry())
  }
}
