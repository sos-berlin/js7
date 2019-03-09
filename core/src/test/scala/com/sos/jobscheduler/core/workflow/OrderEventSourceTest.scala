package com.sos.jobscheduler.core.workflow

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.base.utils.ScalaUtils._
import com.sos.jobscheduler.core.message.ProblemCodeMessages
import com.sos.jobscheduler.core.problems.CancelStartedOrderProblem
import com.sos.jobscheduler.core.workflow.OrderEventHandler.FollowUp
import com.sos.jobscheduler.core.workflow.OrderEventSourceTest._
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.command.CancelMode
import com.sos.jobscheduler.data.event.{<-:, KeyedEvent}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderCancelationMarked, OrderCanceled, OrderCatched, OrderCoreEvent, OrderDetachable, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStopped, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression.{Equal, NumericConstant, OrderReturnCode}
import com.sos.jobscheduler.data.workflow.instructions.{Execute, ExplicitEnd, Gap, Goto, If, IfNonZeroReturnCodeGoto, TryInstruction}
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.position.BranchId.{Catch_, Else, Then, Try_}
import com.sos.jobscheduler.data.workflow.position.Position
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
      Valid(Some(disruptedOrder.id <-: OrderMoved(Position(1)))))  // Move to same InstructionNr, and set Order.Ready
  }

  "if" - {
    val workflow = Workflow.of(TestWorkflowId,
      executeScript,                                  // 0
      If(Equal(OrderReturnCode, NumericConstant(0)),  // 1
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
      If(Equal(OrderReturnCode, NumericConstant(0)),        // 1
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
        OrderForked.Child("🥕", orderId / "🥕", MapDiff.empty),
        OrderForked.Child("🍋", orderId / "🍋", MapDiff.empty))),
      orderId <-: OrderDetachable,
      orderId <-: OrderTransferredToMaster))

    assert(process.run(orderId / "🥕") == List(
      orderId / "🥕" <-: OrderProcessingStarted,
      orderId / "🥕" <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      orderId / "🥕" <-: OrderMoved(Position(0) / "fork+🥕" % 1),
      orderId / "🥕" <-: OrderDetachable,
      orderId / "🥕" <-: OrderTransferredToMaster))

    assert(process.step(orderId).isEmpty)  // Nothing to join

    assert(process.run(orderId / "🍋") == List(
      orderId / "🍋" <-: OrderProcessingStarted,
      orderId / "🍋" <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      orderId / "🍋" <-: OrderMoved(Position(0) / "fork+🍋" % 1),
      orderId / "🍋" <-: OrderDetachable,
      orderId / "🍋" <-: OrderTransferredToMaster,
      orderId <-: OrderJoined(MapDiff.empty, Outcome.succeeded)))
    assert(process.step(orderId) == Some(orderId <-: OrderMoved(Position(1))))

    assert(process.step(orderId) == Some(orderId <-: OrderForked(List(
      OrderForked.Child("🥕", orderId / "🥕", MapDiff.empty),
      OrderForked.Child("🍋", orderId / "🍋", MapDiff.empty)))))

    assert(process.run(orderId / "🥕") == List(
      orderId / "🥕" <-: OrderAttachable(TestAgentRefPath),
      orderId / "🥕" <-: OrderTransferredToAgent(TestAgentRefPath),
      orderId / "🥕" <-: OrderProcessingStarted,
      orderId / "🥕" <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      orderId / "🥕" <-: OrderMoved(Position(1) / "fork+🥕" % 1),
      orderId / "🥕" <-: OrderDetachable,
      orderId / "🥕" <-: OrderTransferredToMaster))

    assert(process.step(orderId).isEmpty)  // Nothing to join

    assert(process.run(orderId / "🍋") == List(
      orderId / "🍋" <-: OrderAttachable(TestAgentRefPath),
      orderId / "🍋" <-: OrderTransferredToAgent(TestAgentRefPath),
      orderId / "🍋" <-: OrderProcessingStarted,
      orderId / "🍋" <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      orderId / "🍋" <-: OrderMoved(Position(1) / "fork+🍋" % 1),
      orderId / "🍋" <-: OrderDetachable,
      orderId / "🍋" <-: OrderTransferredToMaster,
      orderId <-: OrderJoined(MapDiff.empty, Outcome.succeeded)))

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
        "B" @:   IfNonZeroReturnCodeGoto("C"), // 5
                 TryInstruction(               // 6
                   Workflow.of(executeScript),  // 6/0:0
                   Workflow.of(executeScript))) // 6/1:0
      val eventSource = newWorkflowEventSource(workflow, List(succeededOrder, failedOrder))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(0)) == Valid(Position(0)))    // Job
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1)) == Valid(Position(6) / Try_ % 0)) // success, next instruction was try
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(2)) == Valid(Position(2)))    // Gap
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(3)) == Valid(Position(3)))    // Job
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(4)) == Valid(Position(4)))    // ExplicitEnd
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(5)) == Valid(Position(6) / Try_ % 0)) // success, next instruction was try
      assert(eventSource.applyMoveInstructions(failedOrder    withPosition Position(5)) == Valid(Position(3)))    // failure
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(6)) == Valid(Position(6) / Try_ % 0))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(7)) == Valid(Position(7)))    // ImplicitEnd
      eventSource.applyMoveInstructions(succeededOrder withInstructionNr 99).isInvalid
    }

    "Jump loops are detected" in {
      val workflow = Workflow.of(
        "A" @: Goto("B"),           // 0
        "B" @: Goto("A"),           // 1
        "C" @: IfNonZeroReturnCodeGoto("A"))   // 2
      val eventSource = newWorkflowEventSource(workflow, List(succeededOrder, failedOrder))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(0)) == Invalid(Problem("Order:SUCCESS is in a workflow loop: 1 B: goto A --> 0 A: goto B")))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1)) == Invalid(Problem("Order:SUCCESS is in a workflow loop: 0 A: goto B --> 1 B: goto A")))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(2)) == Valid(Position(3)))
      assert(eventSource.applyMoveInstructions(failedOrder    withPosition Position(2)) == Invalid(Problem("Order:FAILED is in a workflow loop: 0 A: goto B --> 1 B: goto A")))
    }

    "Job, Fork" in {
      val eventSource = newWorkflowEventSource(ForkWorkflow, List(succeededOrder, failedOrder))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(0)) == Valid(Position(0)))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1)) == Valid(Position(1)))
    }

    "In forked order" in {
      val eventSource = newWorkflowEventSource(ForkWorkflow, List(succeededOrder, failedOrder))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1) / "fork+🥕" % 1) == Valid(Position(1) / "fork+🥕" % 1))
    }
  }

  "Canceled order" - {
    val orderForked = OrderForked(List(
      OrderForked.Child("🥕", OrderId("ORDER/🥕"), MapDiff.empty),
      OrderForked.Child("🍋", OrderId("ORDER/🍋"), MapDiff.empty)))

    "Order.cancel.isEmpty" - {  // Order is not marked as being canceled
      "Fresh Detached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Fresh(None))
        val eventSource = new OrderEventSource(Map(TestWorkflowId -> ForkWorkflow).toChecked, Map(order.id -> order))
        assert(eventSource.nextEvent(order.id) == Valid(Some(order.id <-: OrderStarted)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = true ) == Valid(Some(OrderDetachable)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = false) == Valid(Some(OrderCanceled)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = true ) == Valid(Some(OrderDetachable)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = false) == Valid(Some(OrderCanceled)))
      }

      "Fresh Attached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Fresh(None), Outcome.succeeded, Some(Order.Attached(TestAgentRefPath)))
        val eventSource = new OrderEventSource(Map(TestWorkflowId -> ForkWorkflow).toChecked, Map(order.id -> order))
        assert(eventSource.nextEvent(order.id) == Valid(Some(order.id <-: OrderStarted)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = true ) == Valid(Some(OrderDetachable)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = false) == Valid(Some(OrderCancelationMarked(CancelMode.NotStarted))))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = true ) == Valid(Some(OrderDetachable)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = false) == Valid(Some(OrderCancelationMarked(CancelMode.FreshOrStarted))))
      }

      "Ready Detached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready)
        val eventSource = new OrderEventSource(Map(TestWorkflowId -> ForkWorkflow).toChecked, Map(order.id -> order))
        assert(eventSource.nextEvent(order.id) == Valid(Some(order.id <-: orderForked)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = true ) == Invalid(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = false) == Invalid(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = true ) == Valid(Some(OrderDetachable)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = false) == Valid(Some(OrderCanceled)))
      }

      "Ready Attached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready, Outcome.succeeded, Some(Order.Attached(TestAgentRefPath)))
        val eventSource = new OrderEventSource(Map(TestWorkflowId -> ForkWorkflow).toChecked, Map(order.id -> order))
        assert(eventSource.nextEvent(order.id) == Valid(Some(order.id <-: orderForked)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = true ) == Invalid(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = false) == Invalid(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = true ) == Valid(Some(OrderDetachable)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = false) == Valid(Some(OrderCancelationMarked(CancelMode.FreshOrStarted))))
      }

      "Processing Attached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Processing, Outcome.succeeded, Some(Order.Attached(TestAgentRefPath)))
        val eventSource = new OrderEventSource(Map(TestWorkflowId -> ForkWorkflow).toChecked, Map(order.id -> order))
        assert(eventSource.nextEvent(order.id) == Valid(None))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = true ) == Invalid(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = false) == Invalid(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = true ) == Valid(Some(OrderCancelationMarked(CancelMode.FreshOrStarted))))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = false) == Valid(Some(OrderCancelationMarked(CancelMode.FreshOrStarted))))
      }
    }

    "Order.cancel.isDefined" - {  // Order is marked as being canceled
      "Ready Detached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready, cancel = Some(CancelMode.FreshOrStarted))
        val eventSource = new OrderEventSource(Map(TestWorkflowId -> ForkWorkflow).toChecked, Map(order.id -> order))
        assert(eventSource.nextEvent(order.id) == Valid(Some(order.id <-: OrderCanceled)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = true ) == Invalid(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = false) == Invalid(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = true ) == Valid(None))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = false) == Valid(None))
      }

      "Ready Attached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready, Outcome.succeeded, Some(Order.Attached(TestAgentRefPath)), cancel = Some(CancelMode.FreshOrStarted))
        val eventSource = new OrderEventSource(Map(TestWorkflowId -> ForkWorkflow).toChecked, Map(order.id -> order))
        assert(eventSource.nextEvent(order.id) == Valid(Some(order.id <-: OrderDetachable)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = true ) == Invalid(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = false) == Invalid(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = true ) == Valid(None))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = false) == Valid(None))
      }

      "Processing Attached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Processing, Outcome.succeeded, Some(Order.Attached(TestAgentRefPath)), cancel = Some(CancelMode.FreshOrStarted))
        val eventSource = new OrderEventSource(Map(TestWorkflowId -> ForkWorkflow).toChecked, Map(order.id -> order))
        assert(eventSource.nextEvent(order.id) == Valid(None))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = true ) == Invalid(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.NotStarted    , isAgent = false) == Invalid(CancelStartedOrderProblem(order.id)))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = true ) == Valid(None))
        assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted, isAgent = false) == Valid(None))
      }
    }
  }

  "Try catch" - {
    val workflow = WorkflowParser.parse(
       """define workflow {
         |  try {                                      // 0
         |    try {                                    // 0/0#0
         |      execute agent="/a", executable="/ex";  // 0/0#0/0#0
         |    } catch {
         |      execute agent="/a", executable="/ex";  // 0/0#0/1#0
         |    }
         |  } catch {
         |    execute agent="/a", executable="/ex";    // 0/1#0
         |    try {                                    // 0/1#1
         |      execute agent="/a", executable="/ex";  // 0/1#1/0#0
         |    } catch {
         |      execute agent="/a", executable="/ex";  // 0/1#1/1#0
         |    }
         |  };
         |  execute agent="/a", executable="/ex";      // 1
         |}""".stripMargin).orThrow

    def eventSource(order: Order[Order.State]) = new OrderEventSource(Map(workflow.id -> workflow).toChecked, Map(order.id -> order))
    val failed = Outcome.Failed(ReturnCode(7))

    "Fresh at try instruction -> OrderMoved" in {
      val order = Order(OrderId("ORDER"), workflow.id, Order.Fresh())
      assert(eventSource(order).nextEvent(order.id) == Valid(Some(order.id <-:
        OrderMoved(Position(0) / Try_ % 0 / Try_ % 0))))
    }

    "Ready at instruction -> OrderMoved" in {
      val order = Order(OrderId("ORDER"), workflow.id, Order.Ready)
      assert(eventSource(order).nextEvent(order.id) == Valid(Some(order.id <-:
        OrderMoved(Position(0) / Try_ % 0 / Try_ % 0))))
    }

    "Processed failed in inner try-block -> OrderCatched" in {
      val order = Order(OrderId("ORDER"), workflow.id /: (Position(0) / Try_ % 0 / Try_ % 0), Order.Processed, failed)
      assert(eventSource(order).nextEvent(order.id) == Valid(Some(order.id <-:
        OrderCatched(failed, Position(0) / Try_ % 0 / Catch_ % 0)) ))
    }

    "Processed failed in inner catch-block -> OrderCatched" in {
      val order = Order(OrderId("ORDER"), workflow.id /: (Position(0) / Try_ % 0 / Catch_ % 0), Order.Processed, failed)
      assert(eventSource(order).nextEvent(order.id) == Valid(Some(order.id <-:
        OrderCatched(failed, Position(0) / Catch_ % 0))))
    }

    "Processed failed in outer catch-block -> OrderStopped" in {
      val order = Order(OrderId("ORDER"), workflow.id /: (Position(0) / Catch_ % 0), Order.Processed, failed)
      assert(eventSource(order).nextEvent(order.id) == Valid(Some(order.id <-:
        OrderStopped(failed)) ))
    }

    "Processed failed in try in catch -> OrderCatched" in {
      val order = Order(OrderId("ORDER"), workflow.id /: (Position(0) / Catch_ % 1 / Try_ % 0), Order.Processed, failed)
      assert(eventSource(order).nextEvent(order.id) == Valid(Some(order.id <-:
        OrderCatched(failed, Position(0) / Catch_ % 1 / Catch_ % 0))))
    }

    "Processed failed in catch in catch -> OrderStopped" in {
      val order = Order(OrderId("ORDER"), workflow.id /: (Position(0) / Catch_ % 0), Order.Processed, failed)
      assert(eventSource(order).nextEvent(order.id) == Valid(Some(order.id <-:
        OrderStopped(failed))))
    }

    "Processed failed not in try/catch -> OrderStopped" in {
      val order = Order(OrderId("ORDER"), workflow.id /: Position(1), Order.Processed, failed)
      assert(eventSource(order).nextEvent(order.id) == Valid(Some(order.id <-:
        OrderStopped(failed)) ))
    }
  }
}

object OrderEventSourceTest {
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") ~ "VERSION"
  private val ForkWorkflow = ForkTestSetting.TestWorkflow.withId(TestWorkflowId)
  private val TestAgentRefPath = AgentRefPath("/AGENT")
  private val succeededOrderId = OrderId("SUCCESS")
  private val succeededOrder = Order(succeededOrderId, TestWorkflowId, Order.Processed, outcome = Outcome.Succeeded(ReturnCode.Success))
  private val failedOrder = Order(OrderId("FAILED"), TestWorkflowId, Order.Processed, outcome = Outcome.Succeeded(ReturnCode.StandardFailure))
  private val disruptedOrder = Order(OrderId("DISRUPTED"), TestWorkflowId, Order.Processed, outcome = Outcome.Disrupted(Outcome.Disrupted.JobSchedulerRestarted))

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

    def jobStep(variablesDiff: MapDiff[String, String] = MapDiff.empty, outcome: Outcome = Outcome.Succeeded(ReturnCode.Success)) =
      process.jobStep(orderId, variablesDiff, outcome)

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

    def jobStep(orderId: OrderId, variablesDiff: MapDiff[String, String] = MapDiff.empty, outcome: Outcome = Outcome.succeeded): Unit = {
      update(orderId <-: OrderProcessingStarted)
      update(orderId <-: OrderProcessed(variablesDiff, outcome))
    }

    def processed(orderId: OrderId, variablesDiff: MapDiff[String, String] = MapDiff.empty, outcome: Outcome = Outcome.succeeded): Unit =
      update(orderId <-: OrderProcessed(variablesDiff, outcome))

    def run(orderId: OrderId): List[KeyedEvent[OrderEvent]] =
      step(orderId) match {
        case Some(keyedEvent) => keyedEvent :: (if (idToOrder contains orderId) run(orderId) else Nil)
        case _ => Nil
      }

    def step(orderId: OrderId): Option[KeyedEvent[OrderEvent]] = {
      val keyedEventOption = nextEvent(orderId).orThrow
      keyedEventOption foreach update
      keyedEventOption
    }

    private def nextEvent(orderId: OrderId): Checked[Option[KeyedEvent[OrderEvent]]] = {
      val order = idToOrder(orderId)
      if (order.detaching.isValid)
        Valid(Some(order.id <-: OrderTransferredToMaster))
      else
        (order.state, workflow.instruction(order.position)) match {
          case (_: Order.Ready, _: Execute) =>
            if (order.isDetached)
              Valid(Some(order.id <-: OrderAttachable(TestAgentRefPath)))
            else if (order.isAttaching)
              Valid(Some(order.id <-: OrderTransferredToAgent(TestAgentRefPath)))
            else
              Valid(Some(order.id <-: OrderProcessingStarted))

          case _ if inProcess contains orderId =>
            Valid(Some(orderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded)))

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
