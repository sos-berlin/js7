package js7.data.execution.workflow.instructions

import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.data.execution.workflow.context.StateView
import js7.data.execution.workflow.instructions.RetryExecutorTest._
import js7.data.order.OrderEvent.OrderRetrying
import js7.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import js7.data.value.NamedValues
import js7.data.workflow.instructions.{Gap, Retry, TryInstruction}
import js7.data.workflow.position.{Position, WorkflowPosition}
import js7.data.workflow.{Workflow, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class RetryExecutorTest extends AnyFreeSpec
{
  "toEvents" in {
    assert(toEvents(Position(1)) == Left(Problem("Retry, but not in a catch-block")))
    assert(toEvents(tryPosition) == Left(Problem("Retry, but not in a catch-block")))
    assert(toEvents(tryPosition / "try+0" % 0) == Left(Problem("Retry, but not in a catch-block")))
  }

  "Find try Position and increment BranchId" in {
    assert(toEvents(tryPosition / "catch+0" % 0) == Right(Seq(orderId <-: OrderRetrying(tryPosition / "try+1" % 0))))
    assert(toEvents(tryPosition / "catch+1" % 0) == Right(Seq(orderId <-: OrderRetrying(tryPosition / "try+2" % 0))))
  }

  "Delay" in {
    val delays = 0.s :: 1.s :: 2.s :: 11.5.seconds :: Nil
    assert(toEvents(tryPosition / "catch+0" % 0, delays) == Right(Seq(orderId <-: OrderRetrying(tryPosition / "try+1" % 0, None))))
    assert(toEvents(tryPosition / "catch+1" % 0, delays) == Right(Seq(orderId <-: OrderRetrying(tryPosition / "try+2" % 0, Some(now + 1.s)))))
    assert(toEvents(tryPosition / "catch+2" % 0, delays) == Right(Seq(orderId <-: OrderRetrying(tryPosition / "try+3" % 0, Some(now + 2.s)))))
    assert(toEvents(tryPosition / "catch+3" % 0, delays) == Right(Seq(orderId <-: OrderRetrying(tryPosition / "try+4" % 0, Some(now + 12.s)))))
    assert(toEvents(tryPosition / "catch+4" % 0, delays) == Right(Seq(orderId <-: OrderRetrying(tryPosition / "try+5" % 0, Some(now + 12.s)))))
  }
}

object RetryExecutorTest
{
  private val now = Timestamp("2019-03-08T12:00:00Z")
  private val orderId = OrderId("ORDER-A")
  private val tryPosition = Position(7)
  private val workflowId = WorkflowPath("WORKFLOW") ~ "VERSION"
  private val tryInstruction = TryInstruction(Workflow.empty, Workflow.empty)

  private def toEvents(position: Position, delays: Seq[FiniteDuration] = Nil) = {
    val order = Order(orderId, workflowId /: position, Order.Ready,
      historicOutcomes = HistoricOutcome(Position(0), Outcome.Succeeded(NamedValues.rc(1))) :: Nil)
    val stateView = new StateView.ForTest {
      def isAgent = false

      override def idToOrder = Map(order.id -> order)

      override def instruction(position: WorkflowPosition) =
        if (position == workflowId /: tryPosition) tryInstruction.copy(retryDelays = Some(delays.toVector))
        else Gap.empty
    }
    new RetryExecutor(() => now).toEvents(Retry(), order, stateView)
  }
}
