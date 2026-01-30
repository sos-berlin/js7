package js7.data.execution.workflow.instructions

import js7.base.test.OurTestSuite
import js7.base.time.TimestampForTests.ts
import js7.data.controller.ControllerState
import js7.data.event.EventColl
import js7.data.execution.workflow.instructions.FailExecutorTest.*
import js7.data.order.OrderEvent.{OrderFailed, OrderOutcomeAdded, OrderStarted}
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.workflow.instructions.{EmptyInstruction, Fail, Fork, ForkBranchId}
import js7.data.workflow.position.BranchId.Then
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{InstructionNr, Position}
import js7.data.workflow.{Workflow, WorkflowPath}

/**
  * @author Joacim Zschimmer
  */
final class FailExecutorTest extends OurTestSuite:

  "toEventCalc" - {
    val now = ts"2026-01-21T12:00:00Z"

    "Simple order" - {
      val order = Order(OrderId("ORDER-A"), TestWorkflowId /: Position(1), Order.Ready())
      val workflow = Workflow.of(
        TestWorkflowId,
        EmptyInstruction(),
        Fail())

      "Fresh order will be started" in:
        val controllerState = ControllerState.forTest(
          orders = Seq(order.copy(state = Order.Fresh())),
          workflows = Seq(workflow))
        assert:
          InstructionExecutor.toEventCalc(order.id)
            .calculateEventList(EventColl(controllerState, now)) ==
            Right(List(
              order.id <-: OrderStarted,
              order.id <-: OrderOutcomeAdded(OrderOutcome.failed),
              order.id <-: OrderFailed(Position(1))))

      "Catchable Fail" - {
        "Detached order" in:
          val controllerState = ControllerState.forTest(
            orders = Seq(order),
            workflows = Seq(workflow))
          assert:
            InstructionExecutor.toEventCalc(order.id)
              .calculateEventList(EventColl(controllerState, now)) ==
              Right(List(
                order.id <-: OrderOutcomeAdded(OrderOutcome.failed),
                order.id <-: OrderFailed(Position(1))))

        "Attached order" in:
          pending // TODO AgentState is not accessible from here
        //  val agentState = AgentState.forTest(
        //    orders = Seq(order.copy(attachedState = Some(Order.Attached(AgentPath("AGENT"))))),
        //    workflows = Seq(workflow))
        //  assert:
        //    InstructionExecutor.toEventCalc(order.id)
        //      .calculateEvents(agentState, now)
        //      .map(_.toList) ==
        //      Right(List(
        //        order.id <-: OrderDetachable))
      }
    }

    "Forked order" - {
      val ForkedOrder = Order(OrderId("FORKED"), TestWorkflowId /: Position(1),
        Order.Forked(Vector(
          Order.Forked.Child(ForkBranchId("🥕"), OrderId("FORKED") / "🥕"),
          Order.Forked.Child(ForkBranchId("🍋"), OrderId("FORKED") / "🍋"))))

      val Carrot = Order(ForkedOrder.id / "🥕", TestWorkflowId /: (Position(1) / "fork+🥕" % 2 / Then % 3),
        Order.FailedInFork, parent = Some(ForkedOrder.id))

      val Lemon  = Order(ForkedOrder.id / "🍋", TestWorkflowId /: (Position(1) / "fork+🍋" % 4),
        Order.Ready(), parent = Some(ForkedOrder.id))

      val engineState = ControllerState.forTest(
        orders = Seq(ForkedOrder, Carrot, Lemon),
        workflows = Seq(
          Workflow.of(
            TestWorkflowId,
            EmptyInstruction(),
            Fork.of(
              "🥕" -> Workflow.empty,
              "🍋" -> Workflow.empty))))

      pending // TODO
    }
  }


object FailExecutorTest:
  private val TestWorkflowId = WorkflowPath("WORKFLOW") ~ "VERSION"
