package js7.data.execution.workflow.instructions

import js7.base.test.OurTestSuite
import js7.base.time.WallClock
import js7.data.agent.AgentPath
import js7.data.execution.workflow.instructions.FailExecutorTest.*
import js7.data.order.OrderEvent.{OrderFailedIntermediate_, OrderStarted}
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.state.ControllerTestStateView
import js7.data.workflow.instructions.{Fail, Fork, ForkBranchId}
import js7.data.workflow.position.BranchId.Then
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{InstructionNr, Position, WorkflowPosition}
import js7.data.workflow.{Workflow, WorkflowPath}

/**
  * @author Joacim Zschimmer
  */
final class FailExecutorTest extends OurTestSuite:

  private val failExecutor = new FailExecutor(new InstructionExecutorService(WallClock))

  private lazy val stateView = new ControllerTestStateView(
    idToOrder = Map(
      TestOrder.id -> TestOrder,
      ForkedOrder.id -> ForkedOrder,
      Carrot.id -> Carrot,
      Lemon.id -> Lemon)
  ):
    override def childOrderIsJoinable(order: Order[Order.State], parent: Order[Order.Forked]) =
      Set(Carrot.id, Lemon.id)(order.id)

    override def instruction(position: WorkflowPosition) =
      position match
        case WorkflowPosition(TestWorkflowId, Position(Nil, InstructionNr(1))) =>
          Fork.of(
            "🥕" -> Workflow.empty,
            "🍋" -> Workflow.empty)
        //case WorkflowPosition(TestWorkflowId, Position(BranchPath.Segment(InstructionNr(1), BranchId.Named("fork+🥕")) :: Nil, InstructionNr(2))) =>
        case pos => fail(s"No instruction at $pos")

  "toEvents" - {
    "Fresh order will be started" in:
      assert(failExecutor.toEvents(Fail(), TestOrder.copy(state = Order.Fresh()), stateView) ==
        Right(Seq(TestOrder.id <-: OrderStarted)))

    "Catchable Fail" - {
      "Detached order" in:
        assert(failExecutor.toEvents(Fail(), TestOrder, stateView) ==
          Right(Seq(TestOrder.id <-: OrderFailedIntermediate_(Some(OrderOutcome.failed)))))

      "Attached order" in:
        assert(failExecutor.toEvents(Fail(), TestOrder.copy(attachedState = Some(Order.Attached(AgentPath("AGENT")))), stateView) ==
          Right(Seq(TestOrder.id <-: OrderFailedIntermediate_(Some(OrderOutcome.failed)))))
    }
  }


object FailExecutorTest:

  private val TestWorkflowId = WorkflowPath("WORKFLOW") ~ "VERSION"

  private val TestOrder = Order(OrderId("ORDER-A"), TestWorkflowId /: Position(7), Order.Ready())

  private val ForkedOrder = Order(OrderId("FORKED"), TestWorkflowId /: Position(1),
    Order.Forked(Vector(
      Order.Forked.Child(ForkBranchId("🥕"), OrderId("FORKED") / "🥕"),
      Order.Forked.Child(ForkBranchId("🍋"), OrderId("FORKED") / "🍋"))))

  private val Carrot = Order(ForkedOrder.id / "🥕", TestWorkflowId /: (Position(1) / "fork+🥕" % 2 / Then % 3),
    Order.FailedInFork, parent = Some(ForkedOrder.id))

  private val Lemon  = Order(ForkedOrder.id / "🍋", TestWorkflowId /: (Position(1) / "fork+🍋" % 4),
    Order.Ready(), parent = Some(ForkedOrder.id))
