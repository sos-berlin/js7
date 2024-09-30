package js7.data.execution.workflow

import cats.syntax.option.*
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.time.WallClock
import js7.base.utils.Collections.implicits.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.Problems.CancelStartedOrderProblem
import js7.data.agent.AgentPath
import js7.data.command.CancellationMode.FreshOrStarted
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.event.{<-:, KeyedEvent}
import js7.data.execution.workflow.OrderEventSourceTest.*
import js7.data.execution.workflow.instructions.InstructionExecutorService
import js7.data.job.{PathExecutable, ShellScriptExecutable}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.OrderEvent.OrderResumed.ReplaceHistoricOutcome
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancellationMarked, OrderCancelled, OrderCaught, OrderCoreEvent, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderLocksReleased, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderResumed, OrderResumptionMarked, OrderStarted, OrderSuspended, OrderSuspensionMarked}
import js7.data.order.{HistoricOutcome, Order, OrderEvent, OrderId, OrderMark, OrderOutcome}
import js7.data.problems.{CannotResumeOrderProblem, CannotSuspendOrderProblem, UnreachableOrderPositionProblem}
import js7.data.state.OrderEventHandler.FollowUp
import js7.data.state.{OrderEventHandler, TestStateView}
import js7.data.subagent.Problems.ProcessLostDueToRestartProblem
import js7.data.subagent.SubagentId
import js7.data.value.NamedValues
import js7.data.value.expression.Expression.{BooleanConstant, Equal, LastReturnCode, NumericConstant}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, Fail, Fork, If, LockInstruction, TryInstruction}
import js7.data.workflow.position.BranchId.{Else, Then, catch_, try_}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.test.ForkTestSetting
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath}
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class OrderEventSourceTest extends OurTestSuite:

  import OrderEventSourceTest.instructionExecutorService

  "ProcessLost" - {
    val rawOrder = Order(OrderId("PROCESS-LOST"), TestWorkflowId /: Position(2),
      Order.Processed,
      historicOutcomes = Vector(
        HistoricOutcome(Position(0), OrderOutcome.processLost(ProcessLostDueToRestartProblem))))

    for isAgent <- Seq(false, true) do s"isAgent=$isAgent" in:
      val order = rawOrder.copy(attachedState = isAgent ? Order.Attached(agentPath = TestAgentPath))
      val eventSource = new OrderEventSource(
        TestStateView.of(
          isAgent = isAgent,
          orders = Some(Seq(order)),
          workflows = Some(Seq(ForkWorkflow))))

      assert(eventSource.nextEvents(order.id) == List(
        order.id <-: OrderMoved(order.position)))  // Move to same InstructionNr to repeat the job
  }

  "if" - {
    val workflow = Workflow.of(TestWorkflowId,
      executeScript,                                  // 0
      If(Equal(LastReturnCode, NumericConstant(0)),   // 1
        Workflow.of(executeScript)),                  // 1/then:0
      executeScript)                                  // 2

    "then branch executed" in:
      assert(step(workflow, OrderOutcome.Succeeded(NamedValues.rc(0))) == Seq(OrderMoved(Position(1) / Then % 0)))

    "then branch skipped" in:
      assert(step(workflow, OrderOutcome.Succeeded(NamedValues.rc(1))) == Seq(OrderMoved(Position(2))))

    "again, all events" in:
      val process = new SingleOrderProcess(workflow)
      process.update(OrderAdded(TestWorkflowId))
      process.transferToAgent(TestAgentPath)
      process.update(OrderStarted)
      process.jobStep()
      assert(process.step() == Seq(OrderMoved(Position(1) / Then % 0)))
      process.jobStep()
      assert(process.step() == Seq(OrderMoved(Position(2))))
      process.jobStep()
      assert(process.step() == Seq(OrderMoved(Position(3))))
      process.transferToController()
      assert(process.step() == Seq(OrderFinished()))

    "then branch not executed" in:
      assert(step(workflow, OrderOutcome.Succeeded(NamedValues.rc(1))) == Seq(OrderMoved(Position(2))))
  }

  "if returnCode else" - {
    val workflow = Workflow.of(TestWorkflowId,
      executeScript,                                        // 0
      If(Equal(LastReturnCode, NumericConstant(0)),         // 1
        thenWorkflow = Workflow.of(executeScript),          // 1/0:0
        elseWorkflow = Some(Workflow.of(executeScript))),   // 1/1:0
      executeScript)                                        // 2

    "then branch executed" in:
      assert(step(workflow, OrderOutcome.succeededRC0) == Seq(OrderMoved(Position(1) / Then % 0)))

    "else branch executed" in:
      assert(step(workflow, OrderOutcome.Succeeded(NamedValues.rc(1))) == Seq(OrderMoved(Position(1) / Else % 0)))
  }

  "fork" in:
    val process = new Process(ForkWorkflow)
    val orderId = succeededOrderId

    process.update(orderId <-: OrderAdded(TestWorkflowId))
    process.update(orderId <-: OrderAttachable(TestAgentPath))
    process.update(orderId <-: OrderAttached(TestAgentPath))
    assert(process.run(orderId) == List(
      orderId <-: OrderDetachable,
      orderId <-: OrderDetached,
      orderId <-: OrderStarted,
      orderId <-: OrderForked(Vector(
        OrderForked.Child("🥕", orderId / "🥕"),
        OrderForked.Child("🍋", orderId / "🍋")))))

    assert(process.run(orderId / "🥕") == List(
      orderId / "🥕" <-: OrderAttachable(TestAgentPath),
      orderId / "🥕" <-: OrderAttached(TestAgentPath),
      orderId / "🥕" <-: OrderProcessingStarted(subagentId),
      orderId / "🥕" <-: OrderProcessed(OrderOutcome.succeededRC0),
      orderId / "🥕" <-: OrderMoved(Position(0) / "fork+🥕" % 1),
      orderId / "🥕" <-: OrderDetachable,
      orderId / "🥕" <-: OrderDetached))

    assert(process.step(orderId).isEmpty)  // Nothing to join

    assert(process.run(orderId / "🍋") == List(
      orderId / "🍋" <-: OrderAttachable(TestAgentPath),
      orderId / "🍋" <-: OrderAttached(TestAgentPath),
      orderId / "🍋" <-: OrderProcessingStarted(subagentId),
      orderId / "🍋" <-: OrderProcessed(OrderOutcome.succeededRC0),
      orderId / "🍋" <-: OrderMoved(Position(0) / "fork+🍋" % 1),
      orderId / "🍋" <-: OrderDetachable,
      orderId / "🍋" <-: OrderDetached,
      orderId <-: OrderJoined(OrderOutcome.succeeded)))
    assert(process.step(orderId) == Seq(orderId <-: OrderMoved(Position(1))))

    assert(process.step(orderId) == Seq(orderId <-: OrderForked(Vector(
      OrderForked.Child("🥕", orderId / "🥕"),
      OrderForked.Child("🍋", orderId / "🍋")))))

    assert(process.run(orderId / "🥕") == List(
      orderId / "🥕" <-: OrderAttachable(TestAgentPath),
      orderId / "🥕" <-: OrderAttached(TestAgentPath),
      orderId / "🥕" <-: OrderProcessingStarted(subagentId),
      orderId / "🥕" <-: OrderProcessed(OrderOutcome.succeededRC0),
      orderId / "🥕" <-: OrderMoved(Position(1) / "fork+🥕" % 1),
      orderId / "🥕" <-: OrderDetachable,
      orderId / "🥕" <-: OrderDetached))

    assert(process.step(orderId).isEmpty)  // Nothing to join

    assert(process.run(orderId / "🍋") == List(
      orderId / "🍋" <-: OrderAttachable(TestAgentPath),
      orderId / "🍋" <-: OrderAttached(TestAgentPath),
      orderId / "🍋" <-: OrderProcessingStarted(subagentId),
      orderId / "🍋" <-: OrderProcessed(OrderOutcome.succeededRC0),
      orderId / "🍋" <-: OrderMoved(Position(1) / "fork+🍋" % 1),
      orderId / "🍋" <-: OrderDetachable,
      orderId / "🍋" <-: OrderDetached,
      orderId <-: OrderJoined(OrderOutcome.succeeded)))

    assert(process.step(orderId) == Seq(orderId <-: OrderMoved(Position(2))))
    assert(process.step(orderId) == Seq(orderId <-: OrderAttachable(TestAgentPath)))
    assert(process.step(orderId) == Seq(orderId <-: OrderAttached(TestAgentPath)))
    assert(process.step(orderId) == Seq(orderId <-: OrderProcessingStarted(subagentId)))
    // and so forth...

  "applyMoveInstructions" - {
    for isAgent <- Seq(false, true) do s"isAgent=$isAgent" - {
      "Job, Fork" in:
        val eventSource = newWorkflowEventSource(ForkWorkflow, List(succeededOrder, failedOrder), isAgent = isAgent)
        assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(0)) == Right(Vector.empty))
        assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1)) == Right(Vector.empty))

      "In forked order" in:
        val eventSource = newWorkflowEventSource(ForkWorkflow, List(succeededOrder, failedOrder), isAgent = isAgent)
        assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1) / "fork+🥕" % 1) == Right(Vector.empty))
    }
  }

  "cancel, suspend, resume, false" - {
    val detached = none[Order.AttachedState]
    val attaching = Some(Order.Attaching(TestAgentPath))
    val attached = Some(Order.Attached(TestAgentPath))
    val detaching = Some(Order.Detaching(TestAgentPath))

    val historicOps = Seq(OrderResumed.ReplaceHistoricOutcome(Position(0), OrderOutcome.failed))

    "Order.mark.isEmpty" - {
      val unmarkedOrder = Order(OrderId("ORDER"), TestWorkflowId /: Position(0), Order.Fresh)

      "Fresh" - {
        val freshOrder = unmarkedOrder

        "Detached" in:
          testController(freshOrder, detached): (order, controller) =>
            assert(controller.nextEvents(order.id) == Seq(
              order.id <-: OrderStarted,
              order.id <-: OrderForked(Vector(
                OrderForked.Child(Fork.Branch.Id("🥕"), order.id / "🥕"),
                OrderForked.Child(Fork.Branch.Id("🍋"), order.id / "🍋")))))

          testController(freshOrder, detached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Seq(OrderCancelled)))

          testController(freshOrder, detached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancelled)))

          testController(freshOrder, detached): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Seq(OrderSuspended)))

          testController(freshOrder, detached): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

        "Attaching" in:
          testController(freshOrder, attaching): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testAgent(freshOrder, attaching): (order, agent) =>
            assert(agent.nextEvents(order.id) == Nil)

          testController(freshOrder, attaching): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly))))

          testAgent(freshOrder, attaching): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly))))

          testController(freshOrder, attaching): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testAgent(freshOrder, attaching): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testController(freshOrder, attaching): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Seq(OrderSuspensionMarked())))

          testAgent(freshOrder, attaching): (order, agent) =>
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Seq(OrderSuspensionMarked())))

          testController(freshOrder, attaching): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

          testAgent(freshOrder, attaching): (order, agent) =>
            assert(agent     .resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

        "Attached" in:
          testController(freshOrder, attached): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testAgent(freshOrder, attached): (order, agent) =>
            assert(agent     .nextEvents(order.id) == Seq(order.id <-: OrderDetachable))

          testController(freshOrder, attached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly))))

          testAgent(freshOrder, attached): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Right(Seq(OrderDetachable)))

          testController(freshOrder, attached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testAgent(freshOrder, attached): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderDetachable)))

          testController(freshOrder, attached): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Seq(OrderSuspensionMarked())))

          testAgent(freshOrder, attached): (order, agent) =>
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Seq(OrderDetachable)))

          testController(freshOrder, attached): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

          testAgent(freshOrder, attached): (order, agent) =>
            assert(agent     .resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

        "Detaching" in:
          testController(freshOrder, detaching): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testAgent(freshOrder, detaching): (order, agent) =>
            assert(agent     .nextEvents(order.id) == Nil)

          testController(freshOrder, detaching): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly))))

          testAgent(freshOrder, detaching): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly))))

          testController(freshOrder, detaching): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testAgent(freshOrder, detaching): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testController(freshOrder, detaching): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Seq(OrderSuspensionMarked())))

          testAgent(freshOrder, detaching): (order, agent) =>
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Seq(OrderSuspensionMarked())))

          testController(freshOrder, detaching): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

          testAgent(freshOrder, detaching): (order, agent) =>
            assert(agent     .resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))
      }

      "Ready" - {
        val readyOrder = unmarkedOrder.copy(state = Order.Ready)

        "Detached" in:
          testController(readyOrder, attachedState = detached): (order, controller) =>
            assert(controller.nextEvents(order.id) == Seq(order.id <-: orderForked))

          testController(readyOrder, attachedState = detached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testController(readyOrder, attachedState = detached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancelled)))

        "Attaching" in:
          testController(readyOrder, attaching): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testAgent(readyOrder, attaching): (order, agent) =>
            assert(agent     .nextEvents(order.id) == Nil)

          testController(readyOrder, attaching): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testAgent(readyOrder, attaching): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testController(readyOrder, attaching): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testAgent(readyOrder, attaching): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testController(readyOrder, attaching): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Seq(OrderSuspensionMarked())))

          testAgent(readyOrder, attaching): (order, agent) =>
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Seq(OrderSuspensionMarked())))

          testController(readyOrder, attaching): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

          testAgent(readyOrder, attaching): (order, agent) =>
            assert(agent     .resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

        "Attached" in:
          testController(readyOrder, attached): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .nextEvents(order.id) == Seq(order.id <-: OrderDetachable))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderDetachable)))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Seq(OrderSuspensionMarked())))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Seq(OrderDetachable)))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

        "Detaching" in:
          testController(readyOrder, detaching): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testAgent(readyOrder, detaching): (order, agent) =>
            assert(agent     .nextEvents(order.id) == Nil)

          testController(readyOrder, detaching): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testAgent(readyOrder, detaching): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testController(readyOrder, detaching): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testAgent(readyOrder, detaching): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testController(readyOrder, detaching): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Seq(OrderSuspensionMarked())))

          testAgent(readyOrder, detaching): (order, agent) =>
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Seq(OrderSuspensionMarked())))

          testController(readyOrder, detaching): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

          testAgent(readyOrder, detaching): (order, agent) =>
            assert(agent     .resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))
      }

      "Processing Attached" in:
        val processingOrder = unmarkedOrder.copy(state = Order.Processing(subagentId))
        testController(processingOrder, attached): (order, controller) =>
          assert(controller.nextEvents(order.id) == Nil)

        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .nextEvents(order.id) == Nil)

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.suspend(order.id, SuspensionMode()) == Right(Seq(OrderSuspensionMarked())))

        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .suspend(order.id, SuspensionMode()) == Right(Seq(OrderSuspensionMarked())))

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))
    }

    "OrderMark.Cancelling(FreshOnly)" - {
      val cancellingOrder = Order(OrderId("ORDER"), TestWorkflowId /: Position(0), Order.Fresh,
        mark = Some(OrderMark.Cancelling(CancellationMode.FreshOnly)))

      "Fresh" - {
        val freshOrder = cancellingOrder

        "Detached" in:
          testController(freshOrder, detached): (order, controller) =>
            assert(controller.nextEvents(order.id) == Seq(order.id <-: OrderCancelled))

        "Attached" in:
          testController(freshOrder, attached): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testAgent(freshOrder, attached): (order, agent) =>
            assert(agent     .nextEvents(order.id) == Seq(order.id <-: OrderDetachable))

          testController(freshOrder, attached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Nil))

          testAgent(freshOrder, attached): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Right(Seq(OrderDetachable)))

          testController(freshOrder, attached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(FreshOrStarted(None)))))

          testAgent(freshOrder, attached): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderDetachable)))

          testController(freshOrder, attached): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Left(CannotSuspendOrderProblem))

          testAgent(freshOrder, attached): (order, agent) =>
            assert(agent     .suspend(order.id, SuspensionMode()) == Left(CannotSuspendOrderProblem))

          testController(freshOrder, attached): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

          testAgent(freshOrder, attached): (order, agent) =>
            assert(agent     .resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

          testController(freshOrder, attached): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Left(CannotResumeOrderProblem))

          testAgent(freshOrder, attached): (order, agent) =>
            assert(agent     .resume(order.id, Some(Position(1)), Nil, false) == Left(CannotResumeOrderProblem))
      }
    }

    "OrderMark.Cancelling(FreshOrStarted)" - {
      val cancellingOrder = Order(OrderId("ORDER"), TestWorkflowId /: Position(0), Order.Fresh, mark = Some(OrderMark.Cancelling(CancellationMode.FreshOnly)))

      "Ready" - {
        val readyOrder = cancellingOrder.copy(state = Order.Ready)

        "Detached" in:
          testController(readyOrder, detached): (order, controller) =>
            assert(controller.nextEvents(order.id) == Seq(order.id <-: orderForked))

        "Attached" in:
          testController(readyOrder, attached): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .nextEvents(order.id) == Seq(order.id <-: OrderDetachable))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(FreshOrStarted(None)))))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderDetachable)))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Left(CannotSuspendOrderProblem))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .suspend(order.id, SuspensionMode()) == Left(CannotSuspendOrderProblem))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Left(CannotResumeOrderProblem))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .resume(order.id, Some(Position(1)), Nil, false) == Left(CannotResumeOrderProblem))
      }

      "Processing Attached" in:
        val processingOrder = cancellingOrder.copy(state = Order.Processing(subagentId))

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.nextEvents(order.id) == Nil)

        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .nextEvents(order.id) == Nil)

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(FreshOrStarted(None)))))

        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(FreshOrStarted(None)))))

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.suspend(order.id, SuspensionMode()) == Left(CannotSuspendOrderProblem))

        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .suspend(order.id, SuspensionMode()) == Left(CannotSuspendOrderProblem))

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .resume(order.id, None, Nil, false) == Left(CannotResumeOrderProblem))

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Left(CannotResumeOrderProblem))

        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .resume(order.id, Some(Position(1)), Nil, false) == Left(CannotResumeOrderProblem))
    }

    "OrderMark.Suspending" - {
      val suspendingOrder = Order(OrderId("ORDER"), TestWorkflowId /: Position(0), Order.Ready, mark = Some(OrderMark.Suspending()))

      "Ready" - {
        val readyOrder = suspendingOrder

        "Detached" in:
          testController(readyOrder, detached): (order, controller) =>
            assert(controller.nextEvents(order.id) == Seq(order.id <-: OrderSuspended))

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancelled)))

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(OrderSuspended :: Nil))

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Left(CannotResumeOrderProblem))

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(1)), Seq(ReplaceHistoricOutcome(Position(0), OrderOutcome.succeeded)), false) == Left(CannotResumeOrderProblem))

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.resume(order.id, None, Seq(ReplaceHistoricOutcome(Position(0), OrderOutcome.succeeded)), false) == Left(CannotResumeOrderProblem))

        "Attached" in:
          testController(readyOrder, attached): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .nextEvents(order.id) == Seq(order.id <-: OrderDetachable))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderDetachable)))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Nil))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(OrderDetachable :: Nil))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Left(CannotResumeOrderProblem))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .resume(order.id, Some(Position(1)), Nil, false) == Left(CannotResumeOrderProblem))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.resume(order.id, None, historicOps, false) == Left(CannotResumeOrderProblem))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .resume(order.id, None, historicOps, false) == Left(CannotResumeOrderProblem))
      }

      "Processing Attached" in:
        val processingOrder = suspendingOrder.copy(state = Order.Processing(subagentId))

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.nextEvents(order.id) == Nil)

        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .nextEvents(order.id) == Nil)

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.suspend(order.id, SuspensionMode()) == Right(Nil))

        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .suspend(order.id, SuspensionMode()) == Right(Nil))

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))

        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Left(CannotResumeOrderProblem))

        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .resume(order.id, Some(Position(1)), Nil, false) == Left(CannotResumeOrderProblem))

        testController(processingOrder, attached): (order, controller) =>
          assert(controller.resume(order.id, None, historicOps, false) == Left(CannotResumeOrderProblem))

        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .resume(order.id, None, historicOps, false) == Left(CannotResumeOrderProblem))
    }

    "OrderMark.Resuming isSuspended" - {
      val resumingOrder = Order(OrderId("ORDER"),
        TestWorkflowId /: Position(0),
        Order.Ready,
        mark = Some(OrderMark.Resuming()),
        isSuspended = true)

      "Ready" - {
        val readyOrder = resumingOrder

        "Detached" in:
          testController(readyOrder, detached): (order, controller) =>
            assert(controller.nextEvents(order.id) == Seq(order.id <-: OrderResumed()))

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly) == Left(CancelStartedOrderProblem(order.id)))

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancelled)))

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Seq(OrderSuspended)))

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Right(Seq(OrderResumed())/*should already be happened*/))

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Left(CannotResumeOrderProblem))

        "Attached" in:
          testController(readyOrder, attached): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .nextEvents(order.id) == Seq(order.id <-: OrderResumed()))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderDetachable)))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Seq(OrderSuspensionMarked(SuspensionMode()))))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Seq(OrderDetachable)))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Right(Nil))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .resume(order.id, None, Nil, false) == Right(Nil))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Left(CannotResumeOrderProblem))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .resume(order.id, Some(Position(1)), Nil, false) == Left(CannotResumeOrderProblem))
      }
    }

    "Order.isSuspended" - {
      val suspendedOrder = Order(OrderId("ORDER"), TestWorkflowId /: Position(0), Order.Fresh, isSuspended = true)

      "Fresh" - {
        val freshOrder = suspendedOrder

        "Detached" in:
          testController(freshOrder, detached): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testController(freshOrder, detached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Seq(OrderCancelled)))

          testController(freshOrder, detached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancelled)))

          testController(freshOrder, detached): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Nil))

          testController(freshOrder, detached): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Right(Seq(OrderResumed())))

          testController(freshOrder, detached): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Right(Seq(OrderResumed(Some(Position(1))))))

          testController(freshOrder, detached): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(99)), Nil, false) == Left(UnreachableOrderPositionProblem))

        "Attaching" in:
          testController(freshOrder, attaching): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testAgent(freshOrder, attaching): (order, agent) =>
            assert(agent     .nextEvents(order.id) == Nil)

          testController(freshOrder, attaching): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly))))

          testAgent(freshOrder, attaching): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly))))

          testController(freshOrder, attaching): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testAgent(freshOrder, attaching): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testController(freshOrder, attaching): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Nil))

          testAgent(freshOrder, attaching): (order, agent) =>
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Nil))

          testController(freshOrder, attaching): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))

          testAgent(freshOrder, attaching): (order, agent) =>
            assert(agent     .resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))

          testController(freshOrder, attaching): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Right(Seq(OrderResumptionMarked(Some(Position(1))))))

          testAgent(freshOrder, attaching): (order, agent) =>
            assert(agent     .resume(order.id, Some(Position(1)), Nil, false) == Right(Seq(OrderResumptionMarked(Some(Position(1))))))

          testController(freshOrder, attaching): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(1)), historicOps, false) == Right(Seq(OrderResumptionMarked(Some(Position(1)), historicOps))))

          testAgent(freshOrder, attaching): (order, agent) =>
            assert(agent     .resume(order.id, Some(Position(1)), historicOps, false) == Right(Seq(OrderResumptionMarked(Some(Position(1)), historicOps))))

        "Attached" in:
          testController(freshOrder, attached): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testAgent(freshOrder, attached): (order, agent) =>
            assert(agent     .nextEvents(order.id) == Nil)

          testController(freshOrder, attached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly))))

          testAgent(freshOrder, attached): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Right(Seq(OrderDetachable)))

          testController(freshOrder, attached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testAgent(freshOrder, attached): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderDetachable)))

          testController(freshOrder, attached): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Nil))

          //testAgent(freshOrder, attached): (order, agent) =>
          //  assert(agent     .suspend(order.id, SuspensionMode()) == Right(Seq(OrderDetachable)))

          testController(freshOrder, attached): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))

          testAgent(freshOrder, attached): (order, agent) =>
            assert(agent     .resume(order.id, None, Nil, false) == Right(Seq(OrderResumed())))

          testController(freshOrder, attached): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Right(Seq(OrderResumptionMarked(Some(Position(1))))))

          testAgent(freshOrder, attached): (order, agent) =>
            assert(agent     .resume(order.id, Some(Position(1)), Nil, false) == Right(Seq(OrderResumed(Some(Position(1))))))

        "Detaching" in:
          testController(freshOrder, detaching): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testAgent(freshOrder, detaching): (order, agent) =>
            assert(agent     .nextEvents(order.id) == Nil)

          testController(freshOrder, detaching): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly))))

          testAgent(freshOrder, detaching): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly))))

          testController(freshOrder, detaching): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testAgent(freshOrder, detaching): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testController(freshOrder, detaching): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Nil))

          testAgent(freshOrder, detaching): (order, agent) =>
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Nil))

          testController(freshOrder, detaching): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))

          testAgent(freshOrder, detaching): (order, agent) =>
            assert(agent     .resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))

          testController(freshOrder, detaching): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Right(Seq(OrderResumptionMarked(Some(Position(1))))))

          testAgent(freshOrder, detaching): (order, agent) =>
            assert(agent     .resume(order.id, Some(Position(1)), Nil, false) == Right(Seq(OrderResumptionMarked(Some(Position(1))))))
      }

      "Ready" - {
        val readyOrder = suspendedOrder.copy(state = Order.Ready)

        "Detached" in:
          testController(readyOrder, detached): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly     ) == Left(CancelStartedOrderProblem(order.id)))

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancelled)))

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Nil))

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Right(Seq(OrderResumed())))

          testController(readyOrder, detached): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Right(Seq(OrderResumed(Some(Position(1))))))

        "Attaching" in:
          testController(readyOrder, attaching): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testAgent(readyOrder, attaching): (order, agent) =>
            assert(agent     .nextEvents(order.id) == Nil)

          testController(readyOrder, attaching): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testAgent(readyOrder, attaching): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testController(readyOrder, attaching): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testAgent(readyOrder, attaching): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testController(readyOrder, attaching): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Nil))

          testAgent(readyOrder, attaching): (order, agent) =>
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Nil))

          testController(readyOrder, attaching): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))

          testAgent(readyOrder, attaching): (order, agent) =>
            assert(agent     .resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))

          testController(readyOrder, attaching): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Right(Seq(OrderResumptionMarked(Some(Position(1))))))

          testAgent(readyOrder, attaching): (order, agent) =>
            assert(agent     .resume(order.id, Some(Position(1)), Nil, false) == Right(Seq(OrderResumptionMarked(Some(Position(1))))))

        "Attached" in:
          testController(readyOrder, attached): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .nextEvents(order.id) == Nil)

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderDetachable)))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .resume(order.id, None, Nil, false) == Right(Seq(OrderResumed())))

          testController(readyOrder, attached): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Right(Seq(OrderResumptionMarked(Some(Position(1))))))

          testAgent(readyOrder, attached): (order, agent) =>
            assert(agent     .resume(order.id, Some(Position(1)), Nil, false) == Right(Seq(OrderResumed(Some(Position(1))))))

        "Detaching" in:
          testController(readyOrder, detaching): (order, controller) =>
            assert(controller.nextEvents(order.id) == Nil)

          testAgent(readyOrder, detaching): (order, agent) =>
            assert(agent     .nextEvents(order.id) == Nil)

          testController(readyOrder, detaching): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testAgent(readyOrder, detaching): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))

          testController(readyOrder, detaching): (order, controller) =>
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testAgent(readyOrder, detaching): (order, agent) =>
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))

          testController(readyOrder, detaching): (order, controller) =>
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Nil))

          testAgent(readyOrder, detaching): (order, agent) =>
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Nil))

          testController(readyOrder, detaching): (order, controller) =>
            assert(controller.resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))

          testAgent(readyOrder, detaching): (order, agent) =>
            assert(agent     .resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))

          testController(readyOrder, detaching): (order, controller) =>
            assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Right(Seq(OrderResumptionMarked(Some(Position(1))))))

          testAgent(readyOrder, detaching): (order, agent) =>
            assert(agent     .resume(order.id, Some(Position(1)), Nil, false) == Right(Seq(OrderResumptionMarked(Some(Position(1))))))
      }

      "Processing Attached" in:
        val processingOrder = suspendedOrder.copy(state = Order.Processing(subagentId))
        testController(processingOrder, attached): (order, controller) =>
          assert(controller.nextEvents(order.id) == Nil)
        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .nextEvents(order.id) == Nil)
        testController(processingOrder, attached): (order, controller) =>
          assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
        testController(processingOrder, attached): (order, controller) =>
          assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))
        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted()))))
        testController(processingOrder, attached): (order, controller) =>
          assert(controller.suspend(order.id, SuspensionMode()) == Right(Nil))
        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .suspend(order.id, SuspensionMode()) == Right(Nil))
        testController(processingOrder, attached): (order, controller) =>
          assert(controller.resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))
        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .resume(order.id, None, Nil, false) == Right(Seq(OrderResumptionMarked())))
        testController(processingOrder, attached): (order, controller) =>
          assert(controller.resume(order.id, Some(Position(1)), Nil, false) == Right(Seq(OrderResumptionMarked(Some(Position(1))))))
        testAgent(processingOrder, attached): (order, agent) =>
          assert(agent     .resume(order.id, Some(Position(1)), Nil, false) == Right(Seq(OrderResumptionMarked(Some(Position(1))))))
    }

    def testController(templateOrder: Order[Order.State], attachedState: Option[Order.AttachedState])
      (body: (Order[Order.State], OrderEventSource) => Unit)
    : Unit =
      testEventSource(templateOrder, attachedState, isAgent = false)(body)

    def testAgent(templateOrder: Order[Order.State], attachedState: Option[Order.AttachedState])
      (body: (Order[Order.State], OrderEventSource) => Unit)
    : Unit =
      testEventSource(templateOrder, attachedState, isAgent = true)(body)

    def testEventSource(
      templateOrder: Order[Order.State],
      attachedState: Option[Order.AttachedState],
      isAgent: Boolean)
      (body: (Order[Order.State], OrderEventSource) => Unit)
    : Unit =
      val order = templateOrder.copy(attachedState = attachedState)
      body(
        order,
        new OrderEventSource(TestStateView.of(
          isAgent = isAgent,
          orders = Some(Seq(order)),
          workflows = Some(Seq(ForkWorkflow)))))

    "Resume and UnreachableOrderPositionProblem" - {
      val lockPath = LockPath("LOCK")
      lazy val execute = Execute.Anonymous(WorkflowJob(TestAgentPath, ShellScriptExecutable(":")))
      lazy val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1", Vector(
        /*0*/ execute,
        /*1*/ If(BooleanConstant(true),
          Workflow.of(execute),
          Some(Workflow.of(execute))),
        /*2*/ TryInstruction(
          Workflow.of(execute),
          Workflow.of(execute)),
        /*3*/ Fork.of(
          "A" -> Workflow.of(execute),
          "B" -> Workflow.of(execute)),
        /*4*/ LockInstruction.single(lockPath, count = None, Workflow.of(execute))))

      "Same level" in:
        assert(testResume(workflow, Position(0), Position(1)) == Right(Seq(OrderResumed(Some(Position(1))))))

      "Into and out of if-then" in:
        assert(isResumableBackAndForth(workflow, Position(0), Position(1) / BranchId.Then % 0))

      "Into and out of if-else" in:
        assert(isResumableBackAndForth(workflow, Position(0), Position(1) / BranchId.Else % 0))

      "Into and out of try" in:
        assert(isResumableBackAndForth(workflow, Position(0), Position(2) / BranchId.Try_ % 0))
        assert(isResumableBackAndForth(workflow, Position(0), Position(2) / BranchId.try_(3) % 0))

      "Into and out of catch" in:
        assert(isResumableBackAndForth(workflow, Position(0), Position(2) / BranchId.Catch_ % 0))
        assert(isResumableBackAndForth(workflow, Position(0), Position(2) / BranchId.catch_(3) % 0))

      "Into a fork is forbidden" in:
        assert(testResume(workflow, Position(0), Position(3) / BranchId.fork("A") % 0) ==
          Left(UnreachableOrderPositionProblem))

      "Leaving a fork is forbidden" in:
        assert(testResume(workflow, Position(3) / BranchId.fork("A") % 0, Position(0)) ==
          Left(UnreachableOrderPositionProblem))

      "Into a lock is forbidden" in:
        assert(testResume(workflow, Position(0), Position(4) / BranchId.Lock % 0) ==
          Left(UnreachableOrderPositionProblem))

      "Leaving a lock is forbidden" in:
        assert(testResume(workflow, Position(4) / BranchId.Lock % 0, Position(0)) ==
          Left(UnreachableOrderPositionProblem))

      def isResumableBackAndForth(workflow: Workflow, a: Position, b: Position) =
        isResumable(workflow, a, b) && isResumable(workflow, b, a)

      def isResumable(workflow: Workflow, from: Position, to: Position) =
        testResume(workflow, from, to) == Right(OrderResumed(Some(to)) :: Nil)

      def testResume(workflow: Workflow, from: Position, to: Position)
      : Checked[List[OrderEvent.OrderActorEvent]] =
        val order = Order(OrderId("SUSPENDED"), workflow.id /: from, Order.Ready, isSuspended = true)
        def eventSource = new OrderEventSource(TestStateView.of(
          isAgent = false,
          orders = Some(Seq(order)),
          workflows = Some(Seq(workflow)),
          itemStates = Seq(LockState(Lock(lockPath)))))
        eventSource.resume(order.id, Some(to), Nil, false)
    }
  }

  "Failed" in:
    lazy val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1", Vector(Fail()))
    val order = Order(OrderId("ORDER"), workflow.id /: Position(0), Order.Failed)
    val eventSource = new OrderEventSource(TestStateView.of(
      isAgent = false,
      orders = Some(Seq(order)),
      workflows = Some(Seq(workflow))))
    assert(eventSource.nextEvents(order.id) == Nil)
    assert(eventSource.suspend(order.id, SuspensionMode()) == Left(CannotSuspendOrderProblem))
    assert(eventSource.cancel(order.id, CancellationMode.Default) ==
      Right(OrderCancelled :: Nil))
    assert(eventSource.resume(order.id, None, Nil, false) ==
      Right(OrderResumed(None, Nil) :: Nil))

  "Try catch" - {
    val workflow = WorkflowParser.parse(
       """define workflow {
         |  try {                                     // 0
         |    try {                                   // 0/try:0
         |      execute agent="a", executable="ex";   // 0/try:0/try:0
         |    } catch {
         |      execute agent="a", executable="ex";   // 0/try:0/catch:0
         |    }
         |  } catch {
         |    execute agent="a", executable="ex";     // 0/catch:0
         |    try (maxTries=3) {                      // 0/catch:1
         |      execute agent="a", executable="ex";   // 0/catch:1/try:0
         |    } catch {
         |      retry;                                // 0/catch:1/catch:0
         |    }
         |  };
         |  execute agent="a", executable="ex";       // 1
         |}""".stripMargin).orThrow

    def eventSource(order: Order[Order.State]) =
      new OrderEventSource(TestStateView.of(
        isAgent = false,
        orders = Some(Seq(order)),
        workflows = Some(Seq(workflow))))

    val failed7 = OrderOutcome.Failed(NamedValues.rc(7))

    "fail" in:
      val order = Order(OrderId("ORDER"), workflow.id /: Position(0), Order.Fresh)
      def failToPosition(position: Position, uncatchable: Boolean = false) =
        eventSource(order).fail(workflow, order.withPosition(position), outcome = None, uncatchable = uncatchable)

      assert(failToPosition(Position(0)) == Right(OrderFailed(Position(0)) :: Nil))
      assert(failToPosition(Position(0) / BranchId.try_(0) % 0) ==
        Right(OrderCaught(Position(0) / BranchId.catch_(0) % 0) :: Nil))

      var pos = Position(0) / BranchId.try_(0) % 0
      assert(failToPosition(pos / BranchId.try_(0) % 0) ==
        Right(OrderCaught(pos / BranchId.catch_(0) % 0) :: Nil))

      assert(failToPosition(pos / BranchId.catch_(0) % 0) ==
        Right(OrderCaught(Position(0) / BranchId.catch_(0) % 0) :: Nil))

      pos = Position(0) / BranchId.catch_(0) % 1
      assert(failToPosition(pos / BranchId.try_(0) % 0) ==
        Right(OrderCaught(pos / BranchId.catch_(0) % 0) :: Nil))

      assert(failToPosition(pos / BranchId.try_(1) % 0) ==
        Right(OrderCaught(pos / BranchId.catch_(1) % 0) :: Nil))

      assert(failToPosition(pos / BranchId.try_(2) % 0) ==
        Right(OrderFailed(pos / BranchId.try_(2) % 0) :: Nil))

      assert(failToPosition(Position(1)) ==
        Right(OrderFailed(Position(1)) :: Nil))

    "Fresh at try instruction -> OrderMoved" in:
      val order = Order(OrderId("ORDER"), workflow.id /: Position(0), Order.Fresh)
      assert(eventSource(order).nextEvents(order.id) == Seq(order.id <-:
        OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 0)))

    "Ready at instruction -> OrderMoved" in:
      val order = Order(OrderId("ORDER"), workflow.id /: Position(0), Order.Ready)
      assert(eventSource(order).nextEvents(order.id) == Seq(order.id <-:
        OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 0)))

    "Processed failed in inner try-block -> OrderCaught" in:
      val pos = Position(0) / try_(0) % 0 / try_(0) % 0
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
      assert(eventSource(order).nextEvents(order.id) == Seq(order.id <-:
        OrderCaught(Position(0) / try_(0) % 0 / catch_(0) % 0)))

    "Processed failed in inner catch-block -> OrderCaught" in:
      val pos = Position(0) / try_(0) % 0 / catch_(0) % 0
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
      assert(eventSource(order).nextEvents(order.id) == Seq(order.id <-:
        OrderCaught(Position(0) / catch_(0) % 0)))

    "Processed failed in outer catch-block -> OrderFailed" in:
      val pos = Position(0) / catch_(0) % 0
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
      assert(eventSource(order).nextEvents(order.id) == Seq(order.id <-: OrderFailed(pos)))

    "Processed failed in try in catch -> OrderCaught" in:
      val pos = Position(0) / catch_(0) % 1 / try_(0) % 0
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
      assert(eventSource(order).nextEvents(order.id) == Seq(order.id <-:
        OrderCaught(Position(0) / catch_(0) % 1 / catch_(0) % 0)))

    "Processed failed in catch in catch -> OrderFailed" in:
      val pos = Position(0) / catch_(0) % 0
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(Position(0), failed7)))
      assert(eventSource(order).nextEvents(order.id) == Seq(order.id <-: OrderFailed(pos)))

    "Processed failed not in try/catch -> OrderFailed" in:
      val pos = Position(1)
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
      assert(eventSource(order).nextEvents(order.id) == Seq(order.id <-: OrderFailed(pos)))

    "Try catch and fork" in:
      val workflow = WorkflowParser.parse(
         """define workflow {
           |  try
           |    fork(joinIfFailed = true) {
           |      "🥕": {
           |        execute agent="a", executable="ex";   // 0/try:0/fork+🥕:0    // FAILS
           |      },
           |      "🍋": {
           |        execute agent="a", executable="ex";   // 0/try:0/fork+🍋:0    // succeeds
           |      }
           |    }
           |  catch {
           |    execute agent="a", executable="ex";       // 0/catch:0
           |  };
           |  execute agent="a", executable="ex";         // 1
           |}""".stripMargin).orThrow
      var aChild: Order[Order.State] =
        val pos = Position(0) / BranchId.try_(0) % 0 / BranchId.fork("🥕") % 0   // Execute
        Order(OrderId("ORDER|🥕"), workflow.id /: pos, Order.Processed,
          historicOutcomes = Vector(HistoricOutcome(pos, failed7)),
          parent = Some(OrderId("ORDER")))
      var bChild: Order[Order.State] =
        val pos = Position(0) / BranchId.try_(0) % 0 / BranchId.fork("🍋") % 1   // End
        Order(OrderId("ORDER|🍋"), workflow.id /: pos, Order.Ready,
          historicOutcomes = Vector(HistoricOutcome(pos, failed7)),
          parent = Some(OrderId("ORDER")))
      val forkingOrder = Order(OrderId("ORDER"),
        workflow.id /: (Position(0) / BranchId.try_(0) % 0),  // Fork
        Order.Forked(Vector(
          Order.Forked.Child("🥕", aChild.id),
          Order.Forked.Child("🍋", bChild.id))))

      def eventSource = new OrderEventSource(TestStateView.of(
        isAgent = false,
        orders = Some(Seq(forkingOrder, aChild, bChild)),
        workflows = Some(Seq(workflow))))

      val orderFailedInFork = OrderFailedInFork(
        Position(0) / BranchId.try_(0) % 0 / BranchId.fork("🥕") % 0)
      assert(eventSource.nextEvents(aChild.id) == Seq(aChild.id <-: orderFailedInFork))
      aChild = aChild.applyEvent(orderFailedInFork).orThrow
      bChild = bChild.applyEvent(orderFailedInFork).orThrow
      // TODO Is FailedInFork replaceable by Ready and !lastOutcome.isSucceeded?

      val orderJoined = OrderJoined(OrderOutcome.Failed(Some(
        """Order:ORDER|🥕 Failed;
          |Order:ORDER|🍋 Failed"""
          .stripMargin)))
      assert(eventSource.nextEvents(aChild.id      ) == Seq(forkingOrder.id <-: orderJoined))
      assert(eventSource.nextEvents(bChild.id      ) == Seq(forkingOrder.id <-: orderJoined))
      assert(eventSource.nextEvents(forkingOrder.id) == Seq(forkingOrder.id <-: orderJoined))

    "Try catch, fork and lock" in:
      val workflow = WorkflowParser.parse(
         """define workflow {
           |  lock (lock="LOCK") {
           |    try
           |      fork (joinIfFailed=true) {
           |        "🥕": {
           |          execute agent="a", executable="ex";   // 0/lock:0/try:0/fork+🥕:0
           |        },
           |        "🍋": {
           |          lock (lock="LOCK-1") {
           |            lock (lock="LOCK-2") {
           |              execute agent="a", executable="ex";   // 0/lock:0/try:0/fork+🍋:0/lock:0/lock:0
           |            }
           |          }
           |        }
           |      }
           |    catch {
           |      execute agent="a", executable="ex";     // 0/lock:&catch:0
           |    };
           |  }
           |}""".stripMargin).orThrow
      var aChild: Order[Order.State] =
        val pos = Position(0) / BranchId.Lock % 0 / BranchId.try_(0) % 0 / BranchId.fork("🥕") % 0   // Execute
        Order(OrderId("ORDER|🥕"), workflow.id /: pos, Order.Processed,
          historicOutcomes = Vector(HistoricOutcome(pos, failed7)),
          parent = Some(OrderId("ORDER")))
      val bChild =
        val pos = Position(0) / BranchId.Lock % 0 / BranchId.try_(0) % 0 / BranchId.fork("🍋") % 0 / BranchId.Lock % 0 / BranchId.Lock % 0
        Order(OrderId("ORDER|🍋"), workflow.id /: pos, Order.Processed,
          historicOutcomes = Vector(HistoricOutcome(pos, failed7)),
          parent = Some(OrderId("ORDER")))
      val forkingOrder = Order(OrderId("ORDER"), workflow.id /: (Position(0) / BranchId.Lock % 0 / BranchId.try_(0) % 0),  // Fork
        Order.Forked(Vector(
          Order.Forked.Child("🥕", aChild.id),
          Order.Forked.Child("🍋", bChild.id))))
      def liveEventSource = new OrderEventSource(TestStateView.of(
        isAgent = false,
        orders = Some(Seq(forkingOrder, aChild, bChild)),
        workflows = Some(Seq(workflow)),
        itemStates = Seq(
          LockState(Lock(LockPath("LOCK"))),
          LockState(Lock(LockPath("LOCK-1"))),
          LockState(Lock(LockPath("LOCK-2"))))))

      val orderFailedInFork = OrderFailedInFork(Position(0) / BranchId.Lock % 0 / BranchId.try_(0) % 0 / BranchId.fork("🥕") % 0)
      assert(liveEventSource.nextEvents(aChild.id) == Seq(aChild.id <-: orderFailedInFork))
      aChild = aChild.applyEvent(orderFailedInFork).orThrow

      assert(liveEventSource.nextEvents(bChild.id) == Seq(
        bChild.id <-: OrderLocksReleased(List(LockPath("LOCK-2"))),
        bChild.id <-: OrderLocksReleased(List(LockPath("LOCK-1"))),
        bChild.id <-: OrderFailedInFork(
          Position(0) / BranchId.Lock % 0 / BranchId.try_(0) % 0 / BranchId.fork("🍋") % 0)))
  }


object OrderEventSourceTest:

  private implicit val instructionExecutorService: InstructionExecutorService =
    new InstructionExecutorService(WallClock)

  private val TestWorkflowId = WorkflowPath("WORKFLOW") ~ "VERSION"
  private val ForkWorkflow = ForkTestSetting.TestWorkflow.withId(TestWorkflowId)
  private val TestAgentPath = AgentPath("AGENT")
  private val subagentId = SubagentId("SUBAGENT")
  private val succeededOrderId = OrderId("SUCCESS")
  private val succeededOrder = Order(succeededOrderId, TestWorkflowId /: Position(0), Order.Processed,
    historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Succeeded(NamedValues.rc(0)))))
  private val failedOrder = Order(OrderId("FAILED"), TestWorkflowId /: Position(0), Order.Processed,
    historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Failed(NamedValues.rc(1)))))
  private val orderForked = OrderForked(Vector(
    OrderForked.Child("🥕", OrderId("ORDER|🥕")),
    OrderForked.Child("🍋", OrderId("ORDER|🍋"))))

  private val executeScript = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("executable")))

  private def step(workflow: Workflow, outcome: OrderOutcome): Seq[OrderEvent] =
    val process = new SingleOrderProcess(workflow)
    process.update(OrderAdded(workflow.id))
    process.transferToAgent(TestAgentPath)
    process.update(OrderStarted)
    process.jobStep(outcome = outcome)
    process.step()

  final class SingleOrderProcess(val workflow: Workflow, val orderId: OrderId = OrderId("ORDER")):
    private val process = new Process(workflow)

    def transferToAgent(agentPath: AgentPath) =
      update(OrderAttachable(agentPath))
      update(OrderAttached(agentPath))

    def transferToController() =
      update(OrderDetachable)
      update(OrderDetached)

    def jobStep(outcome: OrderOutcome = OrderOutcome.Succeeded(NamedValues.rc(0))) =
      process.jobStep(orderId, outcome)

    def step(): Seq[OrderEvent] =
      process.step(orderId).map(_.event)

    def update(event: OrderEvent) = process.update(orderId <-: event)

  final class Process(workflow: Workflow):
    val idToWorkflow = Map(workflow.id -> workflow)
    val idToOrder = mutable.Map[OrderId, Order[Order.State]]()
    private val eventHandler = new OrderEventHandler(idToWorkflow.checked)
    private val inProcess = mutable.Set.empty[OrderId]

    private def eventSource(isAgent: Boolean) =
      new OrderEventSource(TestStateView.of(
        isAgent = isAgent,
        orders = Some(idToOrder.values),
        workflows = Some(idToWorkflow.values)))

    def jobStep(orderId: OrderId, outcome: OrderOutcome = OrderOutcome.succeeded): Unit =
      update(orderId <-: OrderProcessingStarted(subagentId))
      update(orderId <-: OrderProcessed(outcome))

    def run(orderId: OrderId): List[KeyedEvent[OrderEvent]] =
      step(orderId) match
        case keyedEvents if keyedEvents.nonEmpty =>
          keyedEvents.toList ::: (if idToOrder contains orderId then run(orderId) else Nil)
        case _ => Nil

    def step(orderId: OrderId): Seq[KeyedEvent[OrderEvent]] =
      val keyedEvents = nextEvents(orderId)
      keyedEvents foreach update
      keyedEvents

    private def nextEvents(orderId: OrderId): Seq[KeyedEvent[OrderEvent]] =
      val order = idToOrder(orderId)
      if order.detaching.isRight then
        Seq(order.id <-: OrderDetached)
      else
        (order.state, workflow.instruction(order.position)) match
          case (_: Order.Ready, _: Execute) =>
            if order.isDetached then
              Seq(order.id <-: OrderAttachable(TestAgentPath))
            else if order.isAttaching then
              Seq(order.id <-: OrderAttached(TestAgentPath))
            else
              Seq(order.id <-: OrderProcessingStarted(subagentId))

          case _ if inProcess contains orderId =>
            Seq(orderId <-: OrderProcessed(OrderOutcome.succeededRC0))

          case _ =>
            eventSource(isAgent = order.isAttached).nextEvents(orderId)

    def update(keyedEvent: KeyedEvent[OrderEvent]): Unit =
      val KeyedEvent(orderId, event) = keyedEvent
      event match
        case event: OrderAdded =>
          idToOrder.insert(orderId, Order.fromOrderAdded(orderId, event))

        case event: OrderCoreEvent =>
          processEvent(keyedEvent)
          if !event.isInstanceOf[OrderFinished] then
            idToOrder(orderId) = idToOrder(orderId).applyEvent(event).orThrow

        case _ =>
          sys.error(s"Unhandled: $event")

    private def processEvent(keyedEvent: KeyedEvent[OrderEvent]): Unit =
      keyedEvent match
        case orderId <-: OrderProcessingStarted(_, _, _) =>
          inProcess += orderId

        case orderId <-: (_: OrderProcessed) =>
          inProcess -= orderId

        case _ =>
          eventHandler
            .handleEvent(idToOrder(keyedEvent.key), keyedEvent.event)
            .orThrow
            .foreach:
              case FollowUp.AddChild(derivedOrder) =>
                idToOrder.insert(derivedOrder.id, derivedOrder)

              case FollowUp.Delete(deleteOrderId) =>
                idToOrder -= deleteOrderId

              case o => sys.error(o.toString)

  private def newWorkflowEventSource(
    workflow: Workflow,
    orders: Iterable[Order[Order.State]],
    isAgent: Boolean) =
    new OrderEventSource(TestStateView.of(
      isAgent = isAgent,
      orders = Some(orders),
      workflows = Some(Seq(workflow))))
