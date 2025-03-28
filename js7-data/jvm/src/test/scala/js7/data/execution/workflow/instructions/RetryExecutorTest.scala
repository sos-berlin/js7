package js7.data.execution.workflow.instructions

import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.time.{Timestamp, WallClock}
import js7.data.execution.workflow.instructions.RetryExecutorTest.*
import js7.data.order.OrderEvent.{OrderMoved, OrderRetrying}
import js7.data.order.{HistoricOutcome, Order, OrderId, OrderOutcome}
import js7.data.state.ControllerTestStateView
import js7.data.value.NamedValues
import js7.data.workflow.instructions.{Gap, Retry, TryInstruction}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{Position, WorkflowPosition}
import js7.data.workflow.{Workflow, WorkflowPath}
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class RetryExecutorTest extends OurTestSuite:
  "toEvents" in:
    assert(toEvents(Position(1)) == Left(Problem("Not in a try or catch block")))
    assert(toEvents(tryPosition) == Left(Problem("Not in a try or catch block")))
    assert(toEvents(tryPosition / "try+0" % 0) == Left(Problem("Not in a try or catch block")))

  "Find try Position and increment BranchId" in:
    assert(toEvents(tryPosition / "catch+0" % 0) == Right(Seq(
      orderId <-: OrderRetrying(),
      orderId <-: OrderMoved(tryPosition / "try+1" % 0))))

    assert(toEvents(tryPosition / "catch+1" % 0) == Right(Seq(
      orderId <-: OrderRetrying(),
      orderId <-: OrderMoved(tryPosition / "try+2" % 0))))

  "Delay" in:
    val delays = 0.s :: 1.s :: 2.s :: 11.5.seconds :: Nil
    assert(toEvents(tryPosition / "catch+0" % 0, delays) == Right(Seq(
      orderId <-: OrderRetrying(None),
      orderId <-: OrderMoved(tryPosition / "try+1" % 0))))

    assert(toEvents(tryPosition / "catch+1" % 0, delays) == Right(Seq(
      orderId <-: OrderRetrying(Some(now + 1.s)))))

    assert(toEvents(tryPosition / "catch+2" % 0, delays) == Right(Seq(
      orderId <-: OrderRetrying(Some(now + 2.s)))))

    assert(toEvents(tryPosition / "catch+3" % 0, delays) == Right(Seq(
      orderId <-: OrderRetrying(Some(now + 12.s)))))

    assert(toEvents(tryPosition / "catch+4" % 0, delays) == Right(Seq(
      orderId <-: OrderRetrying(Some(now + 12.s)))))


object RetryExecutorTest:
  private val now = ts"2019-03-08T12:00:00Z"
  private val orderId = OrderId("ORDER-A")
  private val tryPosition = Position(7)
  private val workflowId = WorkflowPath("WORKFLOW") ~ "VERSION"
  private val tryInstruction = TryInstruction(Workflow.empty, Workflow.empty)

  private val retryExecutor = new RetryExecutor(new InstructionExecutorService(WallClock.fixed(now)))

  private def toEvents(position: Position, delays: Seq[FiniteDuration] = Nil) =
    val order = Order(orderId, workflowId /: position, Order.Ready(),
      historicOutcomes = Vector:
        HistoricOutcome(Position(0), OrderOutcome.Succeeded(NamedValues.rc(1))))
    val stateView =
      new ControllerTestStateView(idToOrder = Map(order.id -> order)):
        override def instruction(position: WorkflowPosition) =
          if position == workflowId /: tryPosition then
            tryInstruction.copy(retryDelays = Some(delays.toVector))
          else
            Gap.empty
    retryExecutor.toEvents(Retry(), order, stateView)
