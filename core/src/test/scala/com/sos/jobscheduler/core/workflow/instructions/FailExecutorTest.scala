package com.sos.jobscheduler.core.workflow.instructions

import cats.data.Validated.Valid
import com.sos.jobscheduler.core.workflow.OrderContext
import com.sos.jobscheduler.core.workflow.instructions.FailExecutor.forkPositionOf
import com.sos.jobscheduler.core.workflow.instructions.FailExecutorTest._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent.{OrderDetachable, OrderFailed, OrderFailedCatchable, OrderJoined, OrderStarted}
import com.sos.jobscheduler.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.{Fail, Fork, ImplicitEnd}
import com.sos.jobscheduler.data.workflow.position.BranchId.Then
import com.sos.jobscheduler.data.workflow.position.{InstructionNr, Position, WorkflowPosition}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FailExecutorTest extends FreeSpec
{
  private lazy val context = new OrderContext {
    val idToOrder = Map(TestOrder.id -> TestOrder, ForkedOrder.id -> ForkedOrder, Carrot.id -> Carrot, Lemon.id -> Lemon)

    def childOrderEnded(order: Order[Order.State]) = Set(Carrot.id, Lemon.id)(order.id)

    def instruction(position: WorkflowPosition) = position match {
      case WorkflowPosition(TestWorkflowId, Position(Nil, InstructionNr(1))) =>
        Fork.of(
          "🥕" -> Workflow.empty,
          "🍋" -> Workflow.empty)
      //case WorkflowPosition(TestWorkflowId, Position(BranchPath.Segment(InstructionNr(1), BranchId.Named("fork+🥕")) :: Nil, InstructionNr(2))) =>
      case pos => fail(s"No instruction at $pos")
    }

    def idToWorkflow(id: WorkflowId) = throw new NotImplementedError
  }

  "toEvent" - {
    "Fresh order will be started" in {
      assert(FailExecutor.toEvent(context, TestOrder.copy(state = Order.Fresh()), Fail()) ==
        Valid(Some(TestOrder.id <-: OrderStarted)))
    }

    "Catchable Fail" - {
      "Detached order" in {
        assert(FailExecutor.toEvent(context, TestOrder, Fail()) ==
          Valid(Some(TestOrder.id <-: OrderFailedCatchable(Outcome.Failed(ReturnCode(0))))))
      }

      "Attached order" in {
        assert(FailExecutor.toEvent(context, TestOrder.copy(attachedState = Some(Order.Attached(AgentRefPath("/AGENT")))), Fail()) ==
          Valid(Some(TestOrder.id <-: OrderFailedCatchable(Outcome.Failed(ReturnCode(0))))))
      }
    }

    "Uncatchable Fail" - {
      val fail = Fail(uncatchable = true)

      "Attached order will be detached if fail is uncatchable" in {
        assert(FailExecutor.toEvent(context, TestOrder.copy(attachedState = Some(Order.Attached(AgentRefPath("/AGENT")))), fail) ==
          Valid(Some(TestOrder.id <-: OrderDetachable)))
      }

      "OrderFailed" in {
        assert(FailExecutor.toEvent(context, TestOrder, fail) ==
          Valid(Some(TestOrder.id <-: OrderFailed(Outcome.Failed(ReturnCode(0))))))
      }

      "OrderFailed keeps last Outcome but not the keyValues" in {
        val order = TestOrder.copy(historicOutcomes = HistoricOutcome(Position(0), Outcome.Succeeded(ReturnCode(888), Map("A" -> "AA"))) :: Nil)
        assert(FailExecutor.toEvent(context, order, fail) == Valid(Some(TestOrder.id <-: OrderFailed(Outcome.Failed(ReturnCode(888))))))
      }

      "One fork's child order fails while the other has reached the end" in {
        assert(FailExecutor.toEvent(context, Carrot, fail) ==
          Valid(Some(ForkedOrder.id <-: OrderJoined(Outcome.succeeded))))
      }

      "One fork's child order ends while the other is in state FailedInFork" in {
        assert(EndExecutor.toEvent(context, Lemon, ImplicitEnd()) ==
          Valid(Some(ForkedOrder.id <-: OrderJoined(Outcome.succeeded))))
      }
    }
  }

  "forkPositionOf" in {
    assert(forkPositionOf(Position(1)).isInvalid)
    assert(forkPositionOf(Position(1) / "fork+A" % 2) == Valid(Position(1)))
    assert(forkPositionOf(Position(1) / "fork+A" % 2 / Then % 3) == Valid(Position(1)))
    assert(forkPositionOf(Position(1) / "fork+A" % 2 / Then % 3 / "fork+B" % 4) == Valid(Position(1) / "fork+A" % 2 / Then % 3))
  }
}

object FailExecutorTest
{
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") ~ "VERSION"

  private val TestOrder = Order(OrderId("ORDER-A"), TestWorkflowId /: Position(7), Order.Ready)

  private val ForkedOrder = Order(OrderId("FORKED"), TestWorkflowId /: Position(1),
    Order.Forked(Vector(
      Order.Forked.Child(Fork.Branch.Id("🥕"), OrderId("FORKED") / "🥕"),
      Order.Forked.Child(Fork.Branch.Id("🍋"), OrderId("FORKED") / "🍋"))))

  private val Carrot = Order(ForkedOrder.id / "🥕", TestWorkflowId /: (Position(1) / "fork+🥕" % 2 / Then % 3),
    Order.FailedInFork(Outcome.Failed(ReturnCode(33))), parent = Some(ForkedOrder.id))

  private val Lemon  = Order(ForkedOrder.id / "🍋", TestWorkflowId /: (Position(1) / "fork+🍋" % 4),
    Order.Ready, parent = Some(ForkedOrder.id))
}
