package com.sos.jobscheduler.core.workflow

import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.core.message.ProblemCodeMessages
import com.sos.jobscheduler.core.problems.CancelStartedOrderProblem
import com.sos.jobscheduler.core.workflow.OrderEventHandler.FollowUp
import com.sos.jobscheduler.core.workflow.OrderEventSourceTest._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.event.{<-:, KeyedEvent}
import com.sos.jobscheduler.data.expression.Expression.{Equal, LastReturnCode, NumericConstant}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderCancelationMarked, OrderCanceled, OrderCatched, OrderCoreEvent, OrderDetachable, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStopped, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{HistoricOutcome, Order, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{Execute, ExplicitEnd, Gap, Goto, If, IfFailedGoto, TryInstruction}
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.position.BranchId.{Else, Then, catch_, try_}
import com.sos.jobscheduler.data.workflow.position.{BranchId, Position}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import org.scalatest.FreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class OrderEventSourceTest extends FreeSpec
{
  ProblemCodeMessages.initialize()

  "JobSchedulerRestarted" in {
    val eventSource = new OrderEventSource(
      Map(TestWorkflowId -> ForkWorkflow).toChecked,
      Map(disruptedOrder.id -> disruptedOrder))
    assert(eventSource.nextEvent(disruptedOrder.id) ==
      Some(disruptedOrder.id <-: OrderMoved(disruptedOrder.position)))  // Move to same InstructionNr
  }

  "if" - {
    val workflow = Workflow.of(TestWorkflowId,
      executeScript,                                  // 0
      If(Equal(LastReturnCode, NumericConstant(0)),   // 1
        Workflow.of(executeScript)),                  // 1/0:0
      executeScript)                                  // 2

    "then branch executed" in {
      assert(step(workflow, Outcome.Succeeded(ReturnCode(1))) == Some(OrderMoved(Position(2))))
    }

    "again, all events" in {
      val process = new SingleOrderProcess(workflow)
      process.update(OrderAdded(TestWorkflowId))
      process.transferToAgent(TestAgentRefPath)
      process.update(OrderStarted)
      process.jobStep()
      assert(process.step() == Some(OrderMoved(Position(1) / Then % 0)))
      process.jobStep()
      assert(process.step() == Some(OrderMoved(Position(2))))
      process.jobStep()
      assert(process.step() == Some(OrderMoved(Position(3))))
      process.transferToMaster()
      assert(process.step() == Some(OrderFinished))
    }

    "then branch not executed" in {
      assert(step(workflow, Outcome.Succeeded(ReturnCode(1))) == Some(OrderMoved(Position(2))))
    }
  }

  "if returnCode else" - {
    val workflow = Workflow.of(TestWorkflowId,
      executeScript,                                        // 0
      If(Equal(LastReturnCode, NumericConstant(0)),         // 1
        thenWorkflow = Workflow.of(executeScript),          // 1/0:0
        elseWorkflow = Some(Workflow.of(executeScript))),   // 1/1:0
      executeScript)                                        // 2

    "then branch executed" in {
      assert(step(workflow, Outcome.succeeded) == Some(OrderMoved(Position(1) / Then % 0)))
    }

    "else branch executed" in {
      assert(step(workflow, Outcome.Succeeded(ReturnCode(1))) == Some(OrderMoved(Position(1) / Else % 0)))
    }
  }

  "fork" in {
    val process = new Process(ForkWorkflow)
    val orderId = succeededOrderId

    process.update(orderId <-: OrderAdded(TestWorkflowId))
    process.update(orderId <-: OrderAttachable(TestAgentRefPath))
    process.update(orderId <-: OrderTransferredToAgent(TestAgentRefPath))
    assert(process.run(orderId) == List(
      orderId <-: OrderStarted,
      orderId <-: OrderForked(List(
        OrderForked.Child("🥕", orderId / "🥕"),
        OrderForked.Child("🍋", orderId / "🍋"))),
      orderId <-: OrderDetachable,
      orderId <-: OrderTransferredToMaster))

    assert(process.run(orderId / "🥕") == List(
      orderId / "🥕" <-: OrderProcessingStarted,
      orderId / "🥕" <-: OrderProcessed(Outcome.succeeded),
      orderId / "🥕" <-: OrderMoved(Position(0) / "fork+🥕" % 1),
      orderId / "🥕" <-: OrderDetachable,
      orderId / "🥕" <-: OrderTransferredToMaster))

    assert(process.step(orderId).isEmpty)  // Nothing to join

    assert(process.run(orderId / "🍋") == List(
      orderId / "🍋" <-: OrderProcessingStarted,
      orderId / "🍋" <-: OrderProcessed(Outcome.succeeded),
      orderId / "🍋" <-: OrderMoved(Position(0) / "fork+🍋" % 1),
      orderId / "🍋" <-: OrderDetachable,
      orderId / "🍋" <-: OrderTransferredToMaster,
      orderId <-: OrderJoined(Outcome.succeeded)))
    assert(process.step(orderId) == Some(orderId <-: OrderMoved(Position(1))))

    assert(process.step(orderId) == Some(orderId <-: OrderForked(List(
      OrderForked.Child("🥕", orderId / "🥕"),
      OrderForked.Child("🍋", orderId / "🍋")))))

    assert(process.run(orderId / "🥕") == List(
      orderId / "🥕" <-: OrderAttachable(TestAgentRefPath),
      orderId / "🥕" <-: OrderTransferredToAgent(TestAgentRefPath),
      orderId / "🥕" <-: OrderProcessingStarted,
      orderId / "🥕" <-: OrderProcessed(Outcome.succeeded),
      orderId / "🥕" <-: OrderMoved(Position(1) / "fork+🥕" % 1),
      orderId / "🥕" <-: OrderDetachable,
      orderId / "🥕" <-: OrderTransferredToMaster))

    assert(process.step(orderId).isEmpty)  // Nothing to join

    assert(process.run(orderId / "🍋") == List(
      orderId / "🍋" <-: OrderAttachable(TestAgentRefPath),
      orderId / "🍋" <-: OrderTransferredToAgent(TestAgentRefPath),
      orderId / "🍋" <-: OrderProcessingStarted,
      orderId / "🍋" <-: OrderProcessed(Outcome.succeeded),
      orderId / "🍋" <-: OrderMoved(Position(1) / "fork+🍋" % 1),
      orderId / "🍋" <-: OrderDetachable,
      orderId / "🍋" <-: OrderTransferredToMaster,
      orderId <-: OrderJoined(Outcome.succeeded)))

    assert(process.step(orderId) == Some(orderId <-: OrderMoved(Position(2))))
    assert(process.step(orderId) == Some(orderId <-: OrderAttachable(TestAgentRefPath)))
    assert(process.step(orderId) == Some(orderId <-: OrderTransferredToAgent(TestAgentRefPath)))
    assert(process.step(orderId) == Some(orderId <-: OrderProcessingStarted))
    // and so forth...
  }

  "applyMoveInstructions" - {
    "Goto, IfFailedGoto" in {
      val workflow = Workflow.of(TestWorkflowId,
                 executeScript,  // 0
                 Goto("B"),      // 1
                 Gap(),          // 2
        "C" @:   executeScript,  // 3
        "END" @: ExplicitEnd(),  // 4
        "B" @:   IfFailedGoto("C"), // 5
                 TryInstruction(               // 6
                   Workflow.of(executeScript),  // 6/0:0
                   Workflow.of(executeScript))) // 6/1:0
      val eventSource = newWorkflowEventSource(workflow, List(succeededOrder, failedOrder))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(0)) == Right(Position(0)))    // Job
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1)) == Right(Position(6) / try_(0) % 0)) // success, next instruction was try
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(2)) == Right(Position(2)))    // Gap
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(3)) == Right(Position(3)))    // Job
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(4)) == Right(Position(4)))    // ExplicitEnd
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(5)) == Right(Position(6) / try_(0) % 0)) // success, next instruction was try
      assert(eventSource.applyMoveInstructions(failedOrder    withPosition Position(5)) == Right(Position(3)))    // failure
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(6)) == Right(Position(6) / try_(0) % 0))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(7)) == Right(Position(7)))    // ImplicitEnd
      eventSource.applyMoveInstructions(succeededOrder withInstructionNr 99).isLeft
    }

    "Jump loops are detected" in {
      val workflow = Workflow.of(
        "A" @: Goto("B"),           // 0
        "B" @: Goto("A"),           // 1
        "C" @: IfFailedGoto("A"))   // 2
      val eventSource = newWorkflowEventSource(workflow, List(succeededOrder, failedOrder))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(0)) == Left(Problem("Order:SUCCESS is in a workflow loop: 1 B: goto A --> 0 A: goto B")))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1)) == Left(Problem("Order:SUCCESS is in a workflow loop: 0 A: goto B --> 1 B: goto A")))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(2)) == Right(Position(3)))
      assert(eventSource.applyMoveInstructions(failedOrder    withPosition Position(2)) == Left(Problem("Order:FAILED is in a workflow loop: 0 A: goto B --> 1 B: goto A")))
    }

    "Job, Fork" in {
      val eventSource = newWorkflowEventSource(ForkWorkflow, List(succeededOrder, failedOrder))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(0)) == Right(Position(0)))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1)) == Right(Position(1)))
    }

    "In forked order" in {
      val eventSource = newWorkflowEventSource(ForkWorkflow, List(succeededOrder, failedOrder))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1) / "fork+🥕" % 1) == Right(Position(1) / "fork+🥕" % 1))
    }
  }

  "Canceled order" - {
    val orderForked = OrderForked(List(
      OrderForked.Child("🥕", OrderId("ORDER/🥕")),
      OrderForked.Child("🍋", OrderId("ORDER/🍋"))))

    "Order.cancel.isEmpty" - {  // Order is not marked as being canceled
      "Fresh Detached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Fresh(None))
        val eventSource = new OrderEventSource(Map(TestWorkflowId -> ForkWorkflow).toChecked, Map(order.id -> order))
        assert(eventSource.nextEvent(order.id) == Some(order.id <-: OrderStarted))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = true ) == Right(Some(OrderDetachable)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = false) == Right(Some(OrderCanceled)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = true ) == Right(Some(OrderDetachable)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = false) == Right(Some(OrderCanceled)))
      }

      "Fresh Attached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Fresh(None), attachedState = Some(Order.Attached(TestAgentRefPath)))
        val eventSource = new OrderEventSource(Map(TestWorkflowId -> ForkWorkflow).toChecked, Map(order.id -> order))
        assert(eventSource.nextEvent(order.id) == Some(order.id <-: OrderStarted))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = true ) == Right(Some(OrderDetachable)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = false) == Right(Some(OrderCancelationMarked(CancelMode.NotStarted))))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = true ) == Right(Some(OrderDetachable)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = false) == Right(Some(OrderCancelationMarked(CancelMode.FreshOrStarted))))
      }

      "Ready Detached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready)
        val eventSource = new OrderEventSource(Map(TestWorkflowId -> ForkWorkflow).toChecked, Map(order.id -> order))
        assert(eventSource.nextEvent(order.id) == Some(order.id <-: orderForked))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = true ) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = false) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = true ) == Right(Some(OrderDetachable)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = false) == Right(Some(OrderCanceled)))
      }

      "Ready Attached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready, attachedState = Some(Order.Attached(TestAgentRefPath)))
        val eventSource = new OrderEventSource(Map(TestWorkflowId -> ForkWorkflow).toChecked, Map(order.id -> order))
        assert(eventSource.nextEvent(order.id) == Some(order.id <-: orderForked))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = true ) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = false) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = true ) == Right(Some(OrderDetachable)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = false) == Right(Some(OrderCancelationMarked(CancelMode.FreshOrStarted))))
      }

      "Processing Attached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Processing, attachedState = Some(Order.Attached(TestAgentRefPath)))
        val eventSource = new OrderEventSource(Map(TestWorkflowId -> ForkWorkflow).toChecked, Map(order.id -> order))
        assert(eventSource.nextEvent(order.id) == None)
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = true ) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = false) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = true ) == Right(Some(OrderCancelationMarked(CancelMode.FreshOrStarted))))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = false) == Right(Some(OrderCancelationMarked(CancelMode.FreshOrStarted))))
      }
    }

    "Order.cancel.isDefined" - {  // Order is marked as being canceled
      "Ready Detached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready, cancel = Some(CancelMode.FreshOrStarted))
        val eventSource = new OrderEventSource(Map(TestWorkflowId -> ForkWorkflow).toChecked, Map(order.id -> order))
        assert(eventSource.nextEvent(order.id) == Some(order.id <-: OrderCanceled))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = true ) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = false) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = true ) == Right(None))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = false) == Right(None))
      }

      "Ready Attached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready, attachedState = Some(Order.Attached(TestAgentRefPath)), cancel = Some(CancelMode.FreshOrStarted))
        val eventSource = new OrderEventSource(Map(TestWorkflowId -> ForkWorkflow).toChecked, Map(order.id -> order))
        assert(eventSource.nextEvent(order.id) == Some(order.id <-: OrderDetachable))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = true ) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = false) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = true ) == Right(None))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = false) == Right(None))
      }

      "Processing Attached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Processing, attachedState = Some(Order.Attached(TestAgentRefPath)), cancel = Some(CancelMode.FreshOrStarted))
        val eventSource = new OrderEventSource(Map(TestWorkflowId -> ForkWorkflow).toChecked, Map(order.id -> order))
        assert(eventSource.nextEvent(order.id) == None)
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = true ) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = false) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = true ) == Right(None))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = false) == Right(None))
      }
    }
  }

  "Try catch" - {
    val workflow = WorkflowParser.parse(
       """define workflow {
         |  try {                                      // 0
         |    try {                                    // 0/0:0
         |      execute agent="/a", executable="/ex";  // 0/0:0/0:0
         |    } catch {
         |      execute agent="/a", executable="/ex";  // 0/0:0/1:0
         |    }
         |  } catch {
         |    execute agent="/a", executable="/ex";    // 0/1:0
         |    try {                                    // 0/1:1
         |      execute agent="/a", executable="/ex";  // 0/1:1/0:0
         |    } catch {
         |      execute agent="/a", executable="/ex";  // 0/1:1/1:0
         |    }
         |  };
         |  execute agent="/a", executable="/ex";      // 1
         |}""".stripMargin).orThrow

    def eventSource(order: Order[Order.State]) =
      new OrderEventSource(Map(workflow.id -> workflow).toChecked, Map(order.id -> order))
    val failed = Outcome.Failed(ReturnCode(7))

    "Fresh at try instruction -> OrderMoved" in {
      val order = Order(OrderId("ORDER"), workflow.id, Order.Fresh())
      assert(eventSource(order).nextEvent(order.id) == Some(order.id <-:
        OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 0)))
    }

    "Ready at instruction -> OrderMoved" in {
      val order = Order(OrderId("ORDER"), workflow.id, Order.Ready)
      assert(eventSource(order).nextEvent(order.id) == Some(order.id <-:
        OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 0)))
    }

    "Processed failed in inner try-block -> OrderCatched" in {
      val pos = Position(0) / try_(0) % 0 / try_(0) % 0
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = HistoricOutcome(pos, failed) :: Nil)
      assert(eventSource(order).nextEvent(order.id) == Some(order.id <-:
        OrderCatched(failed, Position(0) / try_(0) % 0 / catch_(0) % 0)))
    }

    "Processed failed in inner catch-block -> OrderCatched" in {
      val pos = Position(0) / try_(0) % 0 / catch_(0) % 0
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = HistoricOutcome(pos, failed) :: Nil)
      assert(eventSource(order).nextEvent(order.id) == Some(order.id <-:
        OrderCatched(failed, Position(0) / catch_(0) % 0)))
    }

    "Processed failed in outer catch-block -> OrderStopped" in {
      val pos = Position(0) / catch_(0) % 0
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = HistoricOutcome(pos, failed) :: Nil)
      assert(eventSource(order).nextEvent(order.id) == Some(order.id <-:
        OrderStopped(failed)))
    }

    "Processed failed in try in catch -> OrderCatched" in {
      val pos = Position(0) / catch_(0) % 1 / try_(0) % 0
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = HistoricOutcome(pos, failed) :: Nil)
      assert(eventSource(order).nextEvent(order.id) == Some(order.id <-:
        OrderCatched(failed, Position(0) / catch_(0) % 1 / catch_(0) % 0)))
    }

    "Processed failed in catch in catch -> OrderStopped" in {
      val order = Order(OrderId("ORDER"), workflow.id /: (Position(0) / catch_(0) % 0), Order.Processed,
        historicOutcomes = HistoricOutcome(Position(0), failed) :: Nil)
      assert(eventSource(order).nextEvent(order.id) == Some(order.id <-:
        OrderStopped(failed)))
    }

    "Processed failed not in try/catch -> OrderStopped" in {
      val pos = Position(1)
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = HistoricOutcome(pos, failed) :: Nil)
      assert(eventSource(order).nextEvent(order.id) == Some(order.id <-:
        OrderStopped(failed)))
    }

    "Try catch and fork" in {
      val workflow = WorkflowParser.parse(
         """define workflow {
           |  try
           |    fork {
           |      "🥕": {
           |        execute agent="/a", executable="/ex";   // 0/try:0/fork+🥕:0    // FAILS
           |      },
           |      "🍋": {
           |        execute agent="/a", executable="/ex";   // 0/try:0/fork+🍋:0    // succeeds
           |      }
           |    }
           |  catch {
           |    execute agent="/a", executable="/ex";       // 0/catch:0
           |  };
           |  execute agent="/a", executable="/ex";         // 1
           |}""".stripMargin).orThrow
      var aChild: Order[Order.State] = {
        val pos = Position(0) / BranchId.try_(0) % 0 / BranchId.fork("🥕") % 0   // Execute
        Order(OrderId("ORDER/🥕"), workflow.id /: pos, Order.Processed,
          historicOutcomes = HistoricOutcome(pos, failed) :: Nil,
          parent = Some(OrderId("ORDER")))
      }
      val bChild = {
        val pos = Position(0) / BranchId.try_(0) % 0 / BranchId.fork("🍋") % 1   // End
        Order(OrderId("ORDER/🍋"), workflow.id /: pos, Order.Ready,
          historicOutcomes = HistoricOutcome(pos, failed) :: Nil,
          parent = Some(OrderId("ORDER")))
      }
      val forkingOrder = Order(OrderId("ORDER"), workflow.id /: (Position(0) / BranchId.try_(0) % 0),  // Fork
        Order.Forked(Vector(
          Order.Forked.Child("🥕", aChild.id),
          Order.Forked.Child("🍋", bChild.id))))
      def liveEventSource = new OrderEventSource(Map(workflow.id -> workflow).toChecked, Map(
        forkingOrder.id -> forkingOrder,
        aChild.id -> aChild,
        bChild.id -> bChild))

      val event = OrderFailedInFork(failed)
      assert(liveEventSource.nextEvent(aChild.id) == Some(aChild.id <-: event))
      aChild = aChild.update(event).orThrow

      assert(liveEventSource.nextEvent(aChild.id      ) == Some(forkingOrder.id <-: OrderJoined(Outcome.Failed(ReturnCode(0)))))
      assert(liveEventSource.nextEvent(bChild.id      ) == Some(forkingOrder.id <-: OrderJoined(Outcome.Failed(ReturnCode(0)))))
      assert(liveEventSource.nextEvent(forkingOrder.id) == Some(forkingOrder.id <-: OrderJoined(Outcome.Failed(ReturnCode(0)))))
    }
  }
}

object OrderEventSourceTest {
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") ~ "VERSION"
  private val ForkWorkflow = ForkTestSetting.TestWorkflow.withId(TestWorkflowId)
  private val TestAgentRefPath = AgentRefPath("/AGENT")
  private val succeededOrderId = OrderId("SUCCESS")
  private val succeededOrder = Order(succeededOrderId, TestWorkflowId, Order.Processed,
    historicOutcomes = HistoricOutcome(Position(0), Outcome.Succeeded(ReturnCode.Success)) :: Nil)
  private val failedOrder = Order(OrderId("FAILED"), TestWorkflowId, Order.Processed,
    historicOutcomes = HistoricOutcome(Position(0), Outcome.Failed(ReturnCode.StandardFailure)) :: Nil)
  private val disruptedOrder = Order(OrderId("DISRUPTED"), TestWorkflowId /: Position(2), Order.Processed,
    historicOutcomes = HistoricOutcome(Position(0), Outcome.Disrupted(Outcome.Disrupted.JobSchedulerRestarted)) :: Nil)

  private val executeScript = Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/executable")))

  private def step(workflow: Workflow, outcome: Outcome): Option[OrderEvent] = {
    val process = new SingleOrderProcess(workflow)
    process.update(OrderAdded(workflow.id))
    process.transferToAgent(TestAgentRefPath)
    process.update(OrderStarted)
    process.jobStep(outcome = outcome)
    process.step()
  }

  final class SingleOrderProcess(val workflow: Workflow, val orderId: OrderId = OrderId("ORDER")) {
    private val process = new Process(workflow)

    def transferToAgent(agentRefPath: AgentRefPath) = {
      update(OrderAttachable(agentRefPath))
      update(OrderTransferredToAgent(agentRefPath))
    }

    def transferToMaster() = {
      update(OrderDetachable)
      update(OrderTransferredToMaster)
    }

    def jobStep(outcome: Outcome = Outcome.Succeeded(ReturnCode.Success)) =
      process.jobStep(orderId, outcome)

    def step(): Option[OrderEvent] =
      process.step(orderId) map (_.event)

    def update(event: OrderEvent) = process.update(orderId <-: event)
  }

  final class Process(workflow: Workflow) {
    val pathToWorkflow = Map(workflow.id -> workflow).toChecked
    val idToOrder = mutable.Map[OrderId, Order[Order.State]]()
    private val eventSource = new OrderEventSource(pathToWorkflow, idToOrder)
    private val eventHandler = new OrderEventHandler(pathToWorkflow, idToOrder)
    private val inProcess = mutable.Set[OrderId]()

    def jobStep(orderId: OrderId, outcome: Outcome = Outcome.succeeded): Unit = {
      update(orderId <-: OrderProcessingStarted)
      update(orderId <-: OrderProcessed(outcome))
    }

    def run(orderId: OrderId): List[KeyedEvent[OrderEvent]] =
      step(orderId) match {
        case Some(keyedEvent) => keyedEvent :: (if (idToOrder contains orderId) run(orderId) else Nil)
        case _ => Nil
      }

    def step(orderId: OrderId): Option[KeyedEvent[OrderEvent]] = {
      val keyedEventOption = nextEvent(orderId)
      keyedEventOption foreach update
      keyedEventOption
    }

    private def nextEvent(orderId: OrderId): Option[KeyedEvent[OrderEvent]] = {
      val order = idToOrder(orderId)
      if (order.detaching.isRight)
        Some(order.id <-: OrderTransferredToMaster)
      else
        (order.state, workflow.instruction(order.position)) match {
          case (_: Order.Ready, _: Execute) =>
            if (order.isDetached)
              Some(order.id <-: OrderAttachable(TestAgentRefPath))
            else if (order.isAttaching)
              Some(order.id <-: OrderTransferredToAgent(TestAgentRefPath))
            else
              Some(order.id <-: OrderProcessingStarted)

          case _ if inProcess contains orderId =>
            Some(orderId <-: OrderProcessed(Outcome.succeeded))

          case _ =>
            eventSource.nextEvent(orderId)
        }
    }

    def update(keyedEvent: KeyedEvent[OrderEvent]): Unit = {
      val KeyedEvent(orderId, event) = keyedEvent
      event match {
        case event: OrderAdded =>
          idToOrder.insert(orderId -> Order.fromOrderAdded(orderId, event))

        case event: OrderCoreEvent =>
          processEvent(keyedEvent)
          if (event != OrderFinished) {
            idToOrder(orderId) = idToOrder(orderId).update(event).orThrow
          }

        case _ =>
          sys.error(s"Unhandled: $event")
      }
    }

    private def processEvent(keyedEvent: KeyedEvent[OrderEvent]): Unit =
      keyedEvent match {
        case orderId <-: OrderProcessingStarted =>
          inProcess += orderId

        case orderId <-: (_: OrderProcessed) =>
          inProcess -= orderId

        case _ =>
          eventHandler.handleEvent(keyedEvent).orThrow foreach {
            case FollowUp.AddChild(derivedOrder) =>
              idToOrder.insert(derivedOrder.id -> derivedOrder)

            case FollowUp.Remove(removeOrderId) =>
              idToOrder -= removeOrderId

            case o => sys.error(o.toString)
          }
      }
  }

  private def newWorkflowEventSource(workflow: Workflow, orders: Iterable[Order[Order.State]]) =
    new OrderEventSource(
      Map(TestWorkflowId -> workflow).toChecked,
      orders toKeyedMap (_.id))
}
