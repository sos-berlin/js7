package js7.data.execution.workflow.instructions

import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentName
import js7.data.execution.workflow.context.OrderContext
import js7.data.execution.workflow.instructions.FailExecutorTest._
import js7.data.order.OrderEvent.{OrderDetachable, OrderFailed, OrderFailedCatchable, OrderJoined, OrderStarted}
import js7.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.instructions.{Fail, Fork, ImplicitEnd}
import js7.data.workflow.position.BranchId.Then
import js7.data.workflow.position.{InstructionNr, Position, WorkflowPosition}
import js7.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FailExecutorTest extends AnyFreeSpec
{
  private lazy val context = new OrderContext {
    val idToOrder = Map(TestOrder.id -> TestOrder, ForkedOrder.id -> ForkedOrder, Carrot.id -> Carrot, Lemon.id -> Lemon).checked

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
        Right(Some(TestOrder.id <-: OrderStarted)))
    }

    "Catchable Fail" - {
      "Detached order" in {
        assert(FailExecutor.toEvent(context, TestOrder, Fail()) ==
          Right(Some(TestOrder.id <-: OrderFailedCatchable(Outcome.failed))))
      }

      "Attached order" in {
        assert(FailExecutor.toEvent(context, TestOrder.copy(attachedState = Some(Order.Attached(AgentName("AGENT")))), Fail()) ==
          Right(Some(TestOrder.id <-: OrderFailedCatchable(Outcome.failed))))
      }
    }

    "Uncatchable Fail" - {
      val fail = Fail(uncatchable = true)

      "Attached order will be detached if fail is uncatchable" in {
        assert(FailExecutor.toEvent(context, TestOrder.copy(attachedState = Some(Order.Attached(AgentName("AGENT")))), fail) ==
          Right(Some(TestOrder.id <-: OrderDetachable)))
      }

      "OrderFailed" in {
        assert(FailExecutor.toEvent(context, TestOrder, fail) ==
          Right(Some(TestOrder.id <-: OrderFailed())))
      }

      "OrderFailed keeps last Outcome but not the namedValues" in {
        val order = TestOrder.copy(
          historicOutcomes = HistoricOutcome(Position(0), Outcome.Succeeded(NamedValues.rc(888) ++ Map( "A" -> StringValue("AA")))) :: Nil)
        assert(FailExecutor.toEvent(context, order, fail) == Right(Some(TestOrder.id <-: OrderFailed())))
      }

      "One fork's child order fails while the other has reached the end" in {
        // Not handled by FailExecutor. OrderEventSource handles this.
        assert(FailExecutor.toEvent(context, Carrot, fail) == Right(None))
      }

      "One fork's child order ends while the other is in state FailedInFork" in {
        assert(EndExecutor.toEvent(context, Lemon, ImplicitEnd()) ==
          Right(Some(ForkedOrder.id <-: OrderJoined(Outcome.succeeded))))
      }
    }
  }
}

object FailExecutorTest
{
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") ~ "VERSION"

  private val TestOrder = Order(OrderId("ORDER-A"), TestWorkflowId /: Position(7), Order.Ready)

  private val ForkedOrder = Order(OrderId("FORKED"), TestWorkflowId /: Position(1),
    Order.Forked(Vector(
      Order.Forked.Child(Fork.Branch.Id("🥕"), OrderId("FORKED") | "🥕"),
      Order.Forked.Child(Fork.Branch.Id("🍋"), OrderId("FORKED") | "🍋"))))

  private val Carrot = Order(ForkedOrder.id | "🥕", TestWorkflowId /: (Position(1) / "fork+🥕" % 2 / Then % 3),
    Order.FailedInFork(Outcome.Failed(NamedValues.rc(33))), parent = Some(ForkedOrder.id))

  private val Lemon  = Order(ForkedOrder.id | "🍋", TestWorkflowId /: (Position(1) / "fork+🍋" % 4),
    Order.Ready, parent = Some(ForkedOrder.id))
}
