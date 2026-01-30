package js7.tests.data

import cats.syntax.option.*
import js7.agent.data.AgentState
import js7.base.problem.Checked
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.time.TimestampForTests.ts
import js7.base.utils.Collections.implicits.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.Problems.CancelStartedOrderProblem
import js7.data.agent.AgentPath
import js7.data.command.CancellationMode.FreshOrStarted
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.controller.ControllerState
import js7.data.event.{Event, EventCalc, EventColl, KeyedEvent, TimeCtx}
import js7.data.execution.workflow.OrderEventSource
import js7.data.job.{PathExecutable, ShellScriptExecutable}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.OrderEvent.OrderResumed.ReplaceHistoricOutcome
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancellationMarked, OrderCancelled, OrderCaught, OrderCoreEvent, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderResumed, OrderResumptionMarked, OrderStarted, OrderSuspended, OrderSuspensionMarked}
import js7.data.order.{HistoricOutcome, Order, OrderEvent, OrderId, OrderMark, OrderOutcome}
import js7.data.plan.PlanFinishedEvent
import js7.data.problems.{CannotResumeOrderProblem, CannotSuspendOrderProblem, UnreachableOrderPositionProblem}
import js7.data.state.OrderEventHandler.FollowUp
import js7.data.state.{EngineState_, OrderEventHandler}
import js7.data.subagent.Problems.ProcessLostDueToRestartProblem
import js7.data.subagent.SubagentId
import js7.data.value.NamedValues
import js7.data.value.expression.Expression.{BooleanConstant, Equal, LastReturnCode, NumericConstant}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, Fail, Fork, ForkBranchId, If, LockInstruction, TryInstruction}
import js7.data.workflow.position.BranchId.{Else, Then, catch_, try_}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.test.ForkTestSetting
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath}
import js7.tests.data.OrderEventSourceTest.*
import scala.collection.mutable

// Placed in js7-tests to get access to AgentState, which resides in js7-agent-data.

final class OrderEventSourceTest extends OurTestSuite:

  "ProcessLost" - {
    val rawOrder = Order(OrderId("PROCESS-LOST"), ForkWorkflow.id /: Position(2),
      Order.Processed,
      historicOutcomes = Vector(
        HistoricOutcome(Position(0), OrderOutcome.processLost(ProcessLostDueToRestartProblem))))

    def testX[S <: EngineState_[S]](order: Order[Order.State], engineState: S) =
      val now = ts"2026-01-21T12:00:00Z"
      assert(OrderEventSource.nextStepEvents(order.id).calculateEventList(EventColl(engineState, now)) ==
        Right(List:
          order.id <-: OrderMoved(order.position))) // Move to same InstructionNr to repeat the job

    for isAgent <- Seq(false, true) do s"isAgent=$isAgent" in :
      if isAgent then
        val order = rawOrder.copy(
          attachedState = Some(Order.Attached(agentPath = TestAgentPath)))
        testX(
          order,
          AgentState.forTest(
            orders = Seq(order),
            workflows = Seq(ForkWorkflow)))
      else
        val order = rawOrder
        testX(
          order,
          ControllerState.forTest(
            orders = Seq(order),
            workflows = Seq(ForkWorkflow)))
  }

  "if" - {
    val workflow = Workflow.of(ForkWorkflow.id,
      executeScript, // 0
      If(Equal(LastReturnCode, NumericConstant(0))): // 1
        Workflow.of(executeScript), // 1/then:0
      executeScript) // 2

    "then branch executed" in :
      assert(step(workflow, OrderOutcome.Succeeded(NamedValues.rc(0))) == Seq(OrderMoved(Position(1) / Then % 0)))

    "then branch skipped" in :
      assert(step(workflow, OrderOutcome.Succeeded(NamedValues.rc(1))) == Seq(OrderMoved(Position(2))))

    "again, all events" in :
      val process = new SingleOrderProcess(workflow)
      process.update(OrderAdded(ForkWorkflow.id))
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

    "then branch not executed" in :
      assert(step(workflow, OrderOutcome.Succeeded(NamedValues.rc(1))) == Seq(OrderMoved(Position(2))))
  }

  "if returnCode else" - {
    val workflow = Workflow.of(ForkWorkflow.id,
      executeScript, // 0
      If(Equal(LastReturnCode, NumericConstant(0))) // 1
        .Then:
          Workflow.of(executeScript) // 1/0:0
        .Else:
          Workflow.of(executeScript), // 1/1:0
      executeScript) // 2

    "then branch executed" in :
      assert(step(workflow, OrderOutcome.succeededRC0) == Seq(OrderMoved(Position(1) / Then % 0)))

    "else branch executed" in :
      assert(step(workflow, OrderOutcome.Succeeded(NamedValues.rc(1))) ==
        Seq(OrderMoved(Position(1) / Else % 0)))
  }

  "fork" in :
    val process = new Process(ForkWorkflow)
    val orderId = succeededOrderId

    process.update(orderId <-: OrderAdded(ForkWorkflow.id))
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

    assert(process.step(orderId).isEmpty) // Nothing to join

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

    assert(process.step(orderId).isEmpty) // Nothing to join

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

  //"applyMoveInstructions" - {
  //  for isAgent <- Seq(false, true) do s"isAgent=$isAgent" - {
  //    "Job, Fork" in:
  //      val engineState = newEngineState(ForkWorkflow, List(succeededOrder, failedOrder), isAgent = isAgent)
  //      assert(EventSource.applyMoveInstructions(succeededOrder.withPosition(Position(0))) == Right(Vector.empty))
  //      assert(eventSource.applyMoveInstructions(succeededOrder.withPosition(Position(1))) == Right(Vector.empty))
  //
  //    "In forked order" in:
  //      val engineState = newEngineState(ForkWorkflow, List(succeededOrder, failedOrder), isAgent = isAgent)
  //      assert(EventSource.applyMoveInstructions(succeededOrder.withPosition(Position(1) / "fork+🥕" % 1)) == Right(Vector.empty))
  //  }
  //}

  "cancel, suspend, resume, false" - {
    val detached = none[Order.AttachedState]
    val attaching = Some(Order.Attaching(TestAgentPath))
    val attached = Some(Order.Attached(TestAgentPath))
    val detaching = Some(Order.Detaching(TestAgentPath))

    val historicOps = Seq(OrderResumed.ReplaceHistoricOutcome(Position(0), OrderOutcome.failed))

    "Order.mark.isEmpty" - {
      val orderId = OrderId("ORDER")
      val unmarkedOrder = Order(orderId, ForkWorkflow.id /: Position(0), Order.Fresh())

      "Fresh" - {
        val freshOrder = unmarkedOrder

        "Detached" in :
          testController(freshOrder, detached): (order, controllerState) =>
            assert:
              OrderEventSource.nextStepEvents(order.id).calculateEventList(EventColl(controllerState, now)) ==
                Right(Seq(
                  order.id <-: OrderStarted,
                  order.id <-: OrderForked(Vector(
                    OrderForked.Child(ForkBranchId("🥕"), order.id / "🥕"),
                    OrderForked.Child(ForkBranchId("🍋"), order.id / "🍋")))))

        testController(freshOrder, detached): (order, controllerState) =>
          assert(OrderEventSource.cancel(order.id, CancellationMode.FreshOnly)
            .calculateEventList(EventColl(controllerState, now))
            == Right(Seq:
            orderId <-: OrderCancelled))

        def toEvents[S <: EngineState_[S]](
          eventCalc: EventCalc[S, OrderCoreEvent],
          engineState: S)
        : Checked[Vector[KeyedEvent[OrderCoreEvent]]] =
          eventCalc.calculateEventList(EventColl(engineState, now)).map(_.toVector)

        assert:
          testController2(freshOrder, detached):
            OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
          == Right(Seq(freshOrder.id <-: OrderCancelled))

        assert:
          testController2(freshOrder, detached):
            OrderEventSource.suspend(freshOrder.id, SuspensionMode())
          == Right(Seq(freshOrder.id <-: OrderSuspended))

        assert:
          testController2(freshOrder, detached):
            OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
          == Left(CannotResumeOrderProblem)

        "Attaching" in :
          assert:
            testController2(freshOrder, attaching):
              OrderEventSource.nextStepEvents(freshOrder.id)
            == Right(Nil)
          assert:
            testAgent2(freshOrder, attaching):
              OrderEventSource.nextStepEvents(freshOrder.id)
            == Right(Nil)

          assert:
            testController2(freshOrder, attaching):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOnly))
          assert:
            testAgent2(freshOrder, attaching):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOnly))

          assert:
            testController2(freshOrder, attaching):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))
          assert:
            testAgent2(freshOrder, attaching):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))

          assert:
            testController2(freshOrder, attaching):
              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
            == Right(Seq:
              orderId <-: OrderSuspensionMarked())
          assert:
            testAgent2(freshOrder, attaching):
              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
            == Right(Seq:
              orderId <-: OrderSuspensionMarked())

          assert:
            testController2(freshOrder, attaching):
              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
            == Left(CannotResumeOrderProblem)
          assert:
            testAgent2(freshOrder, attaching):
              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
            == Left(CannotResumeOrderProblem)

        "Attached" in :
          assert:
            testController2(freshOrder, attached):
              OrderEventSource.nextStepEvents(freshOrder.id)
            == Right(Nil)
          assert:
            testAgent2(freshOrder, attached):
              OrderEventSource.nextStepEvents(freshOrder.id)
            == Right(Seq:
              freshOrder.id <-: OrderDetachable)

          assert:
            testController2(freshOrder, attached):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOnly))
          assert:
            testAgent2(freshOrder, attached):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
            == Right(Seq:
              orderId <-: OrderDetachable)

          assert:
            testController2(freshOrder, attached):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))
          assert:
            testAgent2(freshOrder, attached):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderDetachable)

          assert:
            testController2(freshOrder, attached):
              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
            == Right(Seq:
              orderId <-: OrderSuspensionMarked())
          assert:
            testAgent2(freshOrder, attached):
              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
            == Right(Seq:
              orderId <-: OrderDetachable)

          assert:
            testController2(freshOrder, attached):
              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
            == Left(CannotResumeOrderProblem)
          assert:
            testAgent2(freshOrder, attached):
              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
            == Left(CannotResumeOrderProblem)

        "Detaching" in :
          assert:
            testController2(freshOrder, detaching):
              OrderEventSource.nextStepEvents(freshOrder.id)
            == Right(Nil)
          assert:
            testAgent2(freshOrder, detaching):
              OrderEventSource.nextStepEvents(freshOrder.id)
            == Right(Nil)

          assert:
            testController2(freshOrder, detaching):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOnly))
          assert:
            testAgent2(freshOrder, detaching):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOnly))

          assert:
            testController2(freshOrder, detaching):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))
          assert:
            testAgent2(freshOrder, detaching):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))

          assert:
            testController2(freshOrder, detaching):
              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
            == Right(Seq:
              orderId <-: OrderSuspensionMarked())
          assert:
            testAgent2(freshOrder, detaching):
              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
            == Right(Seq:
              orderId <-: OrderSuspensionMarked())

          assert:
            testController2(freshOrder, detaching):
              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
            == Left(CannotResumeOrderProblem)
          assert:
            testAgent2(freshOrder, detaching):
              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
            == Left(CannotResumeOrderProblem)
      }

      "Ready" - {
        val readyOrder = unmarkedOrder.copy(state = Order.Ready())

        "Detached" in :
          assert:
            testController2(readyOrder, attachedState = detached):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Seq(readyOrder.id <-: orderForked))

          assert:
            testController2(readyOrder, attachedState = detached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))

          assert:
            testController2(readyOrder, attachedState = detached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancelled)

        "Attaching" in :
          assert:
            testController2(readyOrder, attaching):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Nil)
          assert:
            testAgent2(readyOrder, attaching):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Nil)

          assert:
            testController2(readyOrder, attaching):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))
          assert:
            testAgent2(readyOrder, attaching):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))

          assert:
            testController2(readyOrder, attaching):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))
          assert:
            testAgent2(readyOrder, attaching):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))

          assert:
            testController2(readyOrder, attaching):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Seq:
              orderId <-: OrderSuspensionMarked())
          assert:
            testAgent2(readyOrder, attaching):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Seq:
              orderId <-: OrderSuspensionMarked())

          assert:
            testController2(readyOrder, attaching):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Left(CannotResumeOrderProblem)
          assert:
            testAgent2(readyOrder, attaching):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Left(CannotResumeOrderProblem)

        "Attached" in :
          assert:
            testController2(readyOrder, attached):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Nil)
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Seq:
              readyOrder.id <-: OrderDetachable)

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderDetachable)

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Seq(orderId <-: OrderSuspensionMarked()))
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Seq(orderId <-: OrderDetachable))

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Left(CannotResumeOrderProblem)
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Left(CannotResumeOrderProblem)

        "Detaching" in :
          assert:
            testController2(readyOrder, detaching):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Nil)
          assert:
            testAgent2(readyOrder, detaching):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Nil)

          assert:
            testController2(readyOrder, detaching):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))
          assert:
            testAgent2(readyOrder, detaching):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))

          assert:
            testController2(readyOrder, detaching):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq(orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted())))
          assert:
            testAgent2(readyOrder, detaching):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq(orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted())))

          assert:
            testController2(readyOrder, detaching):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Seq(orderId <-: OrderSuspensionMarked()))
          assert:
            testAgent2(readyOrder, detaching):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Seq(orderId <-: OrderSuspensionMarked()))

          assert:
            testController2(readyOrder, detaching):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Left(CannotResumeOrderProblem)
          assert:
            testAgent2(readyOrder, detaching):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Left(CannotResumeOrderProblem)
      }

      "Processing Attached" in :
        val processingOrder = unmarkedOrder.copy(state = Order.Processing(subagentId))
        assert:
          testController2(processingOrder, attached):
            OrderEventSource.nextStepEvents(processingOrder.id)
          == Right(Nil)
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.nextStepEvents(processingOrder.id)
          == Right(Nil)

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOnly)
          == Left(CancelStartedOrderProblem(processingOrder.id))
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOnly)
          == Left(CancelStartedOrderProblem(processingOrder.id))

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOrStarted())
          == Right(Seq:
            processingOrder.id <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOrStarted())
          == Right(Seq:
            processingOrder.id <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.suspend(processingOrder.id, SuspensionMode())
          == Right(Seq:
            processingOrder.id <-: OrderSuspensionMarked())
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.suspend(processingOrder.id, SuspensionMode())
          == Right(Seq:
            processingOrder.id <-: OrderSuspensionMarked())

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.resume(processingOrder.id, None, Nil, false, None)
          == Left(CannotResumeOrderProblem)
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.resume(processingOrder.id, None, Nil, false, None)
          == Left(CannotResumeOrderProblem)
    }

    "OrderMark.Cancelling(FreshOnly)" - {
      val orderId = OrderId("ORDER")
      val cancellingOrder = Order(orderId, ForkWorkflow.id /: Position(0), Order.Fresh(),
        mark = Some(OrderMark.Cancelling(CancellationMode.FreshOnly)))

      "Fresh" - {
        val freshOrder = cancellingOrder

        "Detached" in :
          assert:
            testController2(freshOrder, detached):
              OrderEventSource.nextStepEvents(freshOrder.id)
            == Right(Seq:
              freshOrder.id <-: OrderCancelled)

        "Attached" in :
          assert:
            testController2(freshOrder, attached):
              OrderEventSource.nextStepEvents(freshOrder.id)
            == Right(Nil)
          assert:
            testAgent2(freshOrder, attached):
              OrderEventSource.nextStepEvents(freshOrder.id)
            == Right(Seq:
              freshOrder.id <-: OrderDetachable)

          assert:
            testController2(freshOrder, attached):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
            == Right(Nil)
          assert:
            testAgent2(freshOrder, attached):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
            == Right(Seq(orderId <-: OrderDetachable))

          assert:
            testController2(freshOrder, attached):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq(orderId <-: OrderCancellationMarked(FreshOrStarted(None))))
          assert:
            testAgent2(freshOrder, attached):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq(orderId <-: OrderDetachable))

          assert:
            testController2(freshOrder, attached):
              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
            == Left(CannotSuspendOrderProblem)
          assert:
            testAgent2(freshOrder, attached):
              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
            == Left(CannotSuspendOrderProblem)

          assert:
            testController2(freshOrder, attached):
              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
            == Left(CannotResumeOrderProblem)
          assert:
            testAgent2(freshOrder, attached):
              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
            == Left(CannotResumeOrderProblem)

          assert:
            testController2(freshOrder, attached):
              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
            == Left(CannotResumeOrderProblem)
          assert:
            testAgent2(freshOrder, attached):
              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
            == Left(CannotResumeOrderProblem)
      }
    }

    "OrderMark.Cancelling(FreshOrStarted)" - {
      val orderId = OrderId("ORDER")
      val cancellingOrder = Order(orderId, ForkWorkflow.id /: Position(0), Order.Fresh(),
        mark = Some(OrderMark.Cancelling(CancellationMode.FreshOnly)))

      "Ready" - {
        val readyOrder = cancellingOrder.copy(state = Order.Ready())

        "Detached" in :
          assert:
            testController2(readyOrder, detached):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Seq(readyOrder.id <-: orderForked))

        "Attached" in :
          assert:
            testController2(readyOrder, attached):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Nil)
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Seq:
              readyOrder.id <-: OrderDetachable)

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq(orderId <-: OrderCancellationMarked(FreshOrStarted(None))))
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq(orderId <-: OrderDetachable))

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Left(CannotSuspendOrderProblem)
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Left(CannotSuspendOrderProblem)

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Left(CannotResumeOrderProblem)
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Left(CannotResumeOrderProblem)

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
            == Left(CannotResumeOrderProblem)
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
            == Left(CannotResumeOrderProblem)
      }

      "Processing Attached" in :
        val processingOrder = cancellingOrder.copy(state = Order.Processing(subagentId))

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.nextStepEvents(processingOrder.id)
          == Right(Nil)
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.nextStepEvents(processingOrder.id)
          == Right(Nil)

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOnly)
          == Left(CancelStartedOrderProblem(processingOrder.id))
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOnly)
          == Left(CancelStartedOrderProblem(processingOrder.id))

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOrStarted())
          == Right(Seq(orderId <-: OrderCancellationMarked(FreshOrStarted(None))))
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOrStarted())
          == Right(Seq(orderId <-: OrderCancellationMarked(FreshOrStarted(None))))

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.suspend(processingOrder.id, SuspensionMode())
          == Left(CannotSuspendOrderProblem)
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.suspend(processingOrder.id, SuspensionMode())
          == Left(CannotSuspendOrderProblem)

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.resume(processingOrder.id, None, Nil, false, None)
          == Left(CannotResumeOrderProblem)
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.resume(processingOrder.id, None, Nil, false, None)
          == Left(CannotResumeOrderProblem)

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.resume(processingOrder.id, Some(Position(1)), Nil, false, None)
          == Left(CannotResumeOrderProblem)
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.resume(processingOrder.id, Some(Position(1)), Nil, false, None)
          == Left(CannotResumeOrderProblem)
    }

    "OrderMark.Suspending" - {
      val orderId = OrderId("ORDER")
      val suspendingOrder = Order(orderId, ForkWorkflow.id /: Position(0), Order.Ready(), mark = Some(OrderMark.Suspending()))

      "Ready" - {
        val readyOrder = suspendingOrder

        "Detached" in :
          assert:
            testController2(readyOrder, detached):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Seq:
              readyOrder.id <-: OrderSuspended)

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq(orderId <-: OrderCancelled))

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Seq:
              orderId <-: OrderSuspended)

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked())

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, Some(false))
            == Right(Seq:
              orderId <-: OrderResumptionMarked(restartKilledJob = false))

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.resume(readyOrder.id, None, Nil, true, Some(false))
            == Right(Seq:
              orderId <-: OrderResumptionMarked(asSucceeded = true))

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, Some(true))
            == Right(Seq:
              orderId <-: OrderResumptionMarked(restartKilledJob = true))

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
            == Left(CannotResumeOrderProblem)

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.resume(
                readyOrder.id,
                Some(Position(1)),
                Seq(ReplaceHistoricOutcome(Position(0), OrderOutcome.succeeded)),
                false, None)
            == Left(CannotResumeOrderProblem)

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.resume(
                readyOrder.id, None,
                Seq(ReplaceHistoricOutcome(Position(0), OrderOutcome.succeeded)),
                false, None)
            == Left(CannotResumeOrderProblem)

        "Attached" in :
          assert:
            testController2(readyOrder, attached):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Nil)
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Seq:
              readyOrder.id <-: OrderDetachable)

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderDetachable)

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Nil)
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Seq:
              readyOrder.id <-: OrderDetachable)

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked())
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked())

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
            == Left(CannotResumeOrderProblem)
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
            == Left(CannotResumeOrderProblem)

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, None, historicOps, false, None)
            == Left(CannotResumeOrderProblem)
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, None, historicOps, false, None)
            == Left(CannotResumeOrderProblem)
      }

      "Processing Attached" in :
        val processingOrder = suspendingOrder.copy(state = Order.Processing(subagentId))

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.nextStepEvents(processingOrder.id)
          == Right(Nil)
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.nextStepEvents(processingOrder.id)
          == Right(Nil)

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOnly)
          == Left(CancelStartedOrderProblem(processingOrder.id))

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOrStarted())
          == Right(Seq(processingOrder.id <-: OrderCancellationMarked(CancellationMode.FreshOrStarted())))

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.suspend(processingOrder.id, SuspensionMode())
          == Right(Nil)
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.suspend(processingOrder.id, SuspensionMode())
          == Right(Nil)

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.resume(processingOrder.id, None, Nil, false, None)
          == Right(Seq(processingOrder.id <-: OrderResumptionMarked()))
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.resume(processingOrder.id, None, Nil, false, None)
          == Right(Seq(processingOrder.id <-: OrderResumptionMarked()))

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.resume(processingOrder.id, Some(Position(1)), Nil, false, None)
          == Left(CannotResumeOrderProblem)
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.resume(processingOrder.id, Some(Position(1)), Nil, false, None)
          == Left(CannotResumeOrderProblem)

        assert:
          testController2(processingOrder, attached):
            OrderEventSource.resume(processingOrder.id, None, historicOps, false, None)
          == Left(CannotResumeOrderProblem)
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.resume(processingOrder.id, None, historicOps, false, None)
          == Left(CannotResumeOrderProblem)
    }

    "OrderMark.Resuming isSuspended" - {
      val orderId = OrderId("ORDER")
      val resumingOrder = Order(orderId,
        ForkWorkflow.id /: Position(0),
        Order.Ready(),
        mark = Some(OrderMark.Resuming()),
        isSuspended = true)

      "Ready" - {
        val readyOrder = resumingOrder

        "Detached" in :
          assert:
            testController2(readyOrder, detached):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Seq(
              readyOrder.id <-: OrderResumed(),
              readyOrder.id <-: OrderForked(Vector(
                "🥕" -> readyOrder.id / "🥕",
                "🍋" -> readyOrder.id / "🍋"))))

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancelled)

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Seq:
              orderId <-: OrderSuspended)

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumed()) /*should already be happened*/

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
            == Left(CannotResumeOrderProblem)

        "Attached" in :
          assert:
            testController2(readyOrder, attached):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Nil)
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Seq(
              readyOrder.id <-: OrderResumed(),
              readyOrder.id <-: OrderDetachable))

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderDetachable)

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Seq:
              orderId <-: OrderSuspensionMarked(SuspensionMode()))
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Seq:
              orderId <-: OrderDetachable)

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Right(Nil)
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Right(List:
              readyOrder.id <-: OrderResumed())

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
            == Left(CannotResumeOrderProblem)
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
            == Left(CannotResumeOrderProblem)
      }
    }

    "Order.isSuspended" - {
      val orderId = OrderId("ORDER")
      val suspendedOrder = Order(orderId, ForkWorkflow.id /: Position(0), Order.Fresh(), isSuspended = true)

      "Fresh" - {
        val freshOrder = suspendedOrder

        "Detached" in :
          assert:
            testController2(freshOrder, detached):
              OrderEventSource.nextStepEvents(freshOrder.id)
            == Right(Nil)

          assert:
            testController2(freshOrder, detached):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
            == Right(Seq:
              orderId <-: OrderCancelled)

          assert:
            testController2(freshOrder, detached):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancelled)

          assert:
            testController2(freshOrder, detached):
              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
            == Right(Nil)

          assert:
            testController2(freshOrder, detached):
              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumed())

          assert:
            testController2(freshOrder, detached):
              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumed(Some(Position(1))))

          assert:
            testController2(freshOrder, detached):
              OrderEventSource.resume(freshOrder.id, Some(Position(99)), Nil, false, None)
            == Left(UnreachableOrderPositionProblem)

        "Attaching" in :
          assert:
            testController2(freshOrder, attaching):
              OrderEventSource.nextStepEvents(freshOrder.id)
            == Right(Nil)
          assert:
            testAgent2(freshOrder, attaching):
              OrderEventSource.nextStepEvents(freshOrder.id)
            == Right(Nil)

          assert:
            testController2(freshOrder, attaching):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOnly))
          assert:
            testAgent2(freshOrder, attaching):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOnly))

          assert:
            testController2(freshOrder, attaching):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))
          assert:
            testAgent2(freshOrder, attaching):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))

          assert:
            testController2(freshOrder, attaching):
              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
            == Right(Nil)
          assert:
            testAgent2(freshOrder, attaching):
              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
            == Right(Nil)

          assert:
            testController2(freshOrder, attaching):
              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked())
          assert:
            testAgent2(freshOrder, attaching):
              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked())

          assert:
            testController2(freshOrder, attaching):
              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked(Some(Position(1))))
          assert:
            testAgent2(freshOrder, attaching):
              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked(Some(Position(1))))

          assert:
            testController2(freshOrder, attaching):
              OrderEventSource.resume(freshOrder.id, Some(Position(1)), historicOps, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked(Some(Position(1)), historicOps))
          assert:
            testAgent2(freshOrder, attaching):
              OrderEventSource.resume(freshOrder.id, Some(Position(1)), historicOps, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked(Some(Position(1)), historicOps))

        "Attached" in :
          assert:
            testController2(freshOrder, attached):
              OrderEventSource.nextStepEvents(freshOrder.id)
            == Right(Nil)
          assert:
            testAgent2(freshOrder, attached):
              OrderEventSource.nextStepEvents(freshOrder.id)
            == Right(Nil)

          assert:
            testController2(freshOrder, attached):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOnly))
          assert:
            testAgent2(freshOrder, attached):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
            == Right(Seq:
              orderId <-: OrderDetachable)

          assert:
            testController2(freshOrder, attached):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))
          assert:
            testAgent2(freshOrder, attached):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderDetachable)

          assert:
            testController2(freshOrder, attached):
              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
            == Right(Nil)
          //assert:
          //  testAgent(freshOrder, attached):
          //    OrderEventSource.suspend(freshOrder.id, SuspensionMode())
          //  == Right(Seq(OrderDetachable)))

          assert:
            testController2(freshOrder, attached):
              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked())
          assert:
            testAgent2(freshOrder, attached):
              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumed())

          assert:
            testController2(freshOrder, attached):
              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked(Some(Position(1))))
          assert:
            testAgent2(freshOrder, attached):
              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumed(Some(Position(1))))

        "Detaching" in :
          assert:
            testController2(freshOrder, detaching):
              OrderEventSource.nextStepEvents(freshOrder.id)
            == Right(Nil)
          assert:
            testAgent2(freshOrder, detaching):
              OrderEventSource.nextStepEvents(freshOrder.id)
            == Right(Nil)

          assert:
            testController2(freshOrder, detaching):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOnly))
          assert:
            testAgent2(freshOrder, detaching):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOnly))

          assert:
            testController2(freshOrder, detaching):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))
          assert:
            testAgent2(freshOrder, detaching):
              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))

          assert:
            testController2(freshOrder, detaching):
              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
            == Right(Nil)
          assert:
            testAgent2(freshOrder, detaching):
              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
            == Right(Nil)

          assert:
            testController2(freshOrder, detaching):
              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked())
          assert:
            testAgent2(freshOrder, detaching):
              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked())

          assert:
            testController2(freshOrder, detaching):
              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked(Some(Position(1))))
          assert:
            testAgent2(freshOrder, detaching):
              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked(Some(Position(1))))
      }

      "Ready" - {
        val readyOrder = suspendedOrder.copy(state = Order.Ready())

        "Detached" in :
          assert:
            testController2(readyOrder, detached):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Nil)

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancelled)

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Nil)

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumed())

          assert:
            testController2(readyOrder, detached):
              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumed(Some(Position(1))))

        "Attaching" in :
          assert:
            testController2(readyOrder, attaching):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Nil)
          assert:
            testAgent2(readyOrder, attaching):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Nil)

          assert:
            testController2(readyOrder, attaching):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))
          assert:
            testAgent2(readyOrder, attaching):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))

          assert:
            testController2(readyOrder, attaching):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))
          assert:
            testAgent2(readyOrder, attaching):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))

          assert:
            testController2(readyOrder, attaching):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Nil)
          assert:
            testAgent2(readyOrder, attaching):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Nil)

          assert:
            testController2(readyOrder, attaching):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked())
          assert:
            testAgent2(readyOrder, attaching):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked())

          assert:
            testController2(readyOrder, attaching):
              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked(Some(Position(1))))
          assert:
            testAgent2(readyOrder, attaching):
              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked(Some(Position(1))))

        "Attached" in :
          assert:
            testController2(readyOrder, attached):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Nil)
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Nil)

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq:
              orderId <-: OrderDetachable)

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked())
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumed())

          assert:
            testController2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumptionMarked(Some(Position(1))))
          assert:
            testAgent2(readyOrder, attached):
              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
            == Right(Seq:
              orderId <-: OrderResumed(Some(Position(1))))

        "Detaching" in :
          assert:
            testController2(readyOrder, detaching):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Nil)
          assert:
            testAgent2(readyOrder, detaching):
              OrderEventSource.nextStepEvents(readyOrder.id)
            == Right(Nil)

          assert:
            testController2(readyOrder, detaching):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))
          assert:
            testAgent2(readyOrder, detaching):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
            == Left(CancelStartedOrderProblem(readyOrder.id))

          assert:
            testController2(readyOrder, detaching):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq(readyOrder.id <-: OrderCancellationMarked(CancellationMode.FreshOrStarted())))
          assert:
            testAgent2(readyOrder, detaching):
              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
            == Right(Seq(readyOrder.id <-: OrderCancellationMarked(CancellationMode.FreshOrStarted())))

          assert:
            testController2(readyOrder, detaching):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Nil)
          assert:
            testAgent2(readyOrder, detaching):
              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
            == Right(Nil)

          assert:
            testController2(readyOrder, detaching):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Right(Seq(readyOrder.id <-: OrderResumptionMarked()))
          assert:
            testAgent2(readyOrder, detaching):
              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
            == Right(Seq(readyOrder.id <-: OrderResumptionMarked()))

          assert:
            testController2(readyOrder, detaching):
              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
            == Right(Seq(readyOrder.id <-: OrderResumptionMarked(Some(Position(1)))))
          assert:
            testAgent2(readyOrder, detaching):
              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
            == Right(Seq(readyOrder.id <-: OrderResumptionMarked(Some(Position(1)))))
      }

      "Processing Attached" in :
        val processingOrder = suspendedOrder.copy(state = Order.Processing(subagentId))
        assert:
          testController2(processingOrder, attached):
            OrderEventSource.nextStepEvents(processingOrder.id)
          == Right(Nil)
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.nextStepEvents(processingOrder.id)
          == Right(Nil)
        assert:
          testController2(processingOrder, attached):
            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOnly)
          == Left(CancelStartedOrderProblem(processingOrder.id))
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOnly)
          == Left(CancelStartedOrderProblem(processingOrder.id))
        assert:
          testController2(processingOrder, attached):
            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOrStarted())
          == Right(Seq:
            processingOrder.id <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOrStarted())
          == Right(Seq:
            processingOrder.id <-: OrderCancellationMarked(CancellationMode.FreshOrStarted()))
        assert:
          testController2(processingOrder, attached):
            OrderEventSource.suspend(processingOrder.id, SuspensionMode())
          == Right(Nil)
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.suspend(processingOrder.id, SuspensionMode())
          == Right(Nil)
        assert:
          testController2(processingOrder, attached):
            OrderEventSource.resume(processingOrder.id, None, Nil, false, None)
          == Right(Seq:
            processingOrder.id <-: OrderResumptionMarked())
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.resume(processingOrder.id, None, Nil, false, None)
          == Right(Seq:
            processingOrder.id <-: OrderResumptionMarked())
        assert:
          testController2(processingOrder, attached):
            OrderEventSource.resume(processingOrder.id, Some(Position(1)), Nil, false, None)
          == Right(Seq:
            processingOrder.id <-: OrderResumptionMarked(Some(Position(1))))
        assert:
          testAgent2(processingOrder, attached):
            OrderEventSource.resume(processingOrder.id, Some(Position(1)), Nil, false, None)
          == Right(Seq:
            processingOrder.id <-: OrderResumptionMarked(Some(Position(1))))
    }

    def testController2[E <: Event](
      templateOrder: Order[Order.State],
      attachedState: Option[Order.AttachedState])
      (op: EventCalc[ControllerState, E])
    : Checked[Seq[KeyedEvent[E]]] =
      testController(templateOrder, attachedState): (order, controllerState) =>
        op.calculateEventList(EventColl(controllerState, now))

    def testController[R](
      templateOrder: Order[Order.State],
      attachedState: Option[Order.AttachedState])
      (body: (Order[Order.State], ControllerState) => R)
    : R =
      val order = templateOrder.copy(attachedState = attachedState)
      body(
        order,
        ControllerState.forTest(
          orders = Seq(order),
          workflows = Seq(ForkWorkflow)))

    def testAgent2[E <: Event](
      templateOrder: Order[Order.State],
      attachedState: Option[Order.AttachedState])
      (op: EventCalc[AgentState, E])
    : Checked[Seq[KeyedEvent[E]]] =
      testAgent(templateOrder, attachedState): (order, agentState) =>
        op.calculateEventList(EventColl(agentState, now))

    def testAgent[R](templateOrder: Order[Order.State], attachedState: Option[Order.AttachedState])
                    (body: (Order[Order.State], AgentState) => R)
    : R =
      val order = templateOrder.copy(attachedState = attachedState)
      body(
        order,
        AgentState.forTest(
          orders = Seq(order),
          workflows = Seq(ForkWorkflow)))

    "Resume and UnreachableOrderPositionProblem" - {
      val orderId = OrderId("SUSPENDED")
      val lockPath = LockPath("LOCK")
      lazy val execute = Execute.Anonymous(WorkflowJob(TestAgentPath, ShellScriptExecutable(":")))
      lazy val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1", Vector(
        /*0*/ execute,
        /*1*/ If(BooleanConstant(true))
          .Then(Workflow.of(execute))
          .Else(Workflow.of(execute)),
        /*2*/ TryInstruction(
          Workflow.of(execute),
          Workflow.of(execute)),
        /*3*/ Fork.of(
          "A" -> Workflow.of(execute),
          "B" -> Workflow.of(execute)),
        /*4*/ LockInstruction.single(lockPath):
          Workflow.of(execute)))

      "Same level" in :
        assert(testResume(workflow, Position(0), Position(1)) ==
          Right(Seq(orderId <-: OrderResumed(Some(Position(1))))))

      "Into and out of if-then" in :
        assert(isResumableBackAndForth(workflow, Position(0), Position(1) / BranchId.Then % 0))

      "Into and out of if-else" in :
        assert(isResumableBackAndForth(workflow, Position(0), Position(1) / BranchId.Else % 0))

      "Into and out of try" in :
        assert(isResumableBackAndForth(workflow, Position(0), Position(2) / BranchId.Try_ % 0))
        assert(isResumableBackAndForth(workflow, Position(0), Position(2) / BranchId.try_(3) % 0))

      "Into and out of catch" in :
        assert(isResumableBackAndForth(workflow, Position(0), Position(2) / BranchId.Catch_ % 0))
        assert(isResumableBackAndForth(workflow, Position(0), Position(2) / BranchId.catch_(3) % 0))

      "Into a fork is forbidden" in :
        assert(testResume(workflow, Position(0), Position(3) / BranchId.fork("A") % 0) ==
          Left(UnreachableOrderPositionProblem))

      "Leaving a fork is forbidden" in :
        assert(testResume(workflow, Position(3) / BranchId.fork("A") % 0, Position(0)) ==
          Left(UnreachableOrderPositionProblem))

      "Into a lock is forbidden" in :
        assert(testResume(workflow, Position(0), Position(4) / BranchId.Lock % 0) ==
          Left(UnreachableOrderPositionProblem))

      "Leaving a lock is forbidden" in :
        assert(testResume(workflow, Position(4) / BranchId.Lock % 0, Position(0)) ==
          Left(UnreachableOrderPositionProblem))

      def isResumableBackAndForth(workflow: Workflow, a: Position, b: Position) =
        isResumable(workflow, a, b) && isResumable(workflow, b, a)

      def isResumable(workflow: Workflow, from: Position, to: Position) =
        testResume(workflow, from, to) == Right(List(orderId <-: OrderResumed(Some(to))))

      def testResume(workflow: Workflow, from: Position, to: Position)
      : Checked[List[KeyedEvent[OrderEvent]]] =
        val order = Order(orderId, workflow.id /: from, Order.Ready(), isSuspended = true)
        val controllerState = ControllerState.forTest(
          orders = Seq(order),
          workflows = Seq(workflow),
          itemStates = Seq(LockState(Lock(lockPath))))
        OrderEventSource.resume(order.id, Some(to), Nil, false, None)
          .calculateEventList(EventColl(controllerState, now))
    }
  }

  "Failed" in :
    lazy val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1", Vector(Fail()))
    val orderId = OrderId("ORDER")
    val order = Order(orderId, workflow.id /: Position(0), Order.Failed)
    val controllerState = ControllerState.forTest(
      orders = Seq(order),
      workflows = Seq(workflow))
    assert:
      OrderEventSource.nextStepEvents(order.id)
        .calculateEventList(EventColl(controllerState, now))
        == Right(Nil)
    assert:
      OrderEventSource.suspend(order.id, SuspensionMode())
        .calculateEventList(EventColl(controllerState, now))
        == Left(CannotSuspendOrderProblem)
    assert:
      OrderEventSource.cancel(order.id, CancellationMode.Default)
        .calculateEventList(EventColl(controllerState, now)) ==
        Right(List:
          order.id <-: OrderCancelled)
    assert:
      OrderEventSource.resume(order.id, None, Nil, false, None)
        .calculateEventList(EventColl(controllerState, now)) ==
        Right(List:
          order.id <-: OrderResumed(None, Nil))

  "Try catch" - {
    val orderId = OrderId("ORDER")
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

    def newControllerState(order: Order[Order.State]) =
      ControllerState.forTest(Seq(order), Seq(workflow))

    val failed7 = OrderOutcome.Failed(NamedValues.rc(7))

    "fail" in :
      val order = Order(orderId, workflow.id /: Position(0), Order.Ready())
      def failToPosition(position: Position, uncatchable: Boolean = false) =
        OrderEventSource.fail(order.id, outcome = None, uncatchable = uncatchable)
          .calculateEventList:
            EventColl(newControllerState(order.withPosition(position)), now)


      assert(failToPosition(Position(0)) == Right(List:
        orderId <-: OrderFailed(Position(0))))
      assert(failToPosition(Position(0) / BranchId.try_(0) % 0) ==
        Right(List:
          orderId <-: OrderCaught(Position(0) / BranchId.catch_(0) % 0)))

      var pos = Position(0) / BranchId.try_(0) % 0
      assert(failToPosition(pos / BranchId.try_(0) % 0) ==
        Right(List:
          orderId <-: OrderCaught(pos / BranchId.catch_(0) % 0)))

      assert(failToPosition(pos / BranchId.catch_(0) % 0) ==
        Right(List:
          orderId <-: OrderCaught(Position(0) / BranchId.catch_(0) % 0)))

      pos = Position(0) / BranchId.catch_(0) % 1
      assert(failToPosition(pos / BranchId.try_(0) % 0) ==
        Right(List:
          orderId <-: OrderCaught(pos / BranchId.catch_(0) % 0)))

      assert(failToPosition(pos / BranchId.try_(1) % 0) ==
        Right(List:
          orderId <-: OrderCaught(pos / BranchId.catch_(1) % 0)))

      assert(failToPosition(pos / BranchId.try_(2) % 0) ==
        Right(List:
          orderId <-: OrderFailed(pos / BranchId.try_(2) % 0)))

      assert(failToPosition(Position(1)) ==
        Right(List:
          orderId <-: OrderFailed(Position(1))))

    "Fresh at try instruction -> OrderMoved" in :
      val order = Order(orderId, workflow.id /: Position(0), Order.Fresh())
      assert(OrderEventSource.nextStepEvents(order.id)
        .calculateEventList(EventColl(newControllerState(order), now)) ==
        Right(Seq:
          order.id <-: OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 0)))

    "Ready at instruction -> OrderMoved" in :
      val order = Order(orderId, workflow.id /: Position(0), Order.Ready())
      assert(OrderEventSource.nextStepEvents(order.id)
        .calculateEventList(EventColl(newControllerState(order), now)) ==
        Right(Seq:
          order.id <-: OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 0)))

    "Processed failed in inner try-block -> OrderCaught" in :
      val pos = Position(0) / try_(0) % 0 / try_(0) % 0
      val order = Order(orderId, workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
      assert(OrderEventSource.nextStepEvents(order.id)
        .calculateEventList(EventColl(newControllerState(order), now)) ==
        Right(Seq:
          order.id <-: OrderCaught(Position(0) / try_(0) % 0 / catch_(0) % 0)))

    "Processed failed in inner catch-block -> OrderCaught" in :
      val pos = Position(0) / try_(0) % 0 / catch_(0) % 0
      val order = Order(orderId, workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
      assert(OrderEventSource.nextStepEvents(order.id)
        .calculateEventList(EventColl(newControllerState(order), now)) ==
        Right(Seq:
          order.id <-: OrderCaught(Position(0) / catch_(0) % 0)))

    "Processed failed in outer catch-block -> OrderFailed" in :
      val pos = Position(0) / catch_(0) % 0
      val order = Order(orderId, workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
      assert(OrderEventSource.nextStepEvents(order.id)
        .calculateEventList(EventColl(newControllerState(order), now)) ==
        Right(Seq:
          order.id <-: OrderFailed(pos)))

    "Processed failed in try in catch -> OrderCaught" in :
      val pos = Position(0) / catch_(0) % 1 / try_(0) % 0
      val order = Order(orderId, workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
      assert(OrderEventSource.nextStepEvents(order.id)
        .calculateEventList(EventColl(newControllerState(order), now)) ==
        Right(Seq:
          order.id <-: OrderCaught(Position(0) / catch_(0) % 1 / catch_(0) % 0)))

    "Processed failed in catch in catch -> OrderFailed" in :
      val pos = Position(0) / catch_(0) % 0
      val order = Order(orderId, workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(Position(0), failed7)))
      assert(OrderEventSource.nextStepEvents(order.id)
        .calculateEventList(EventColl(newControllerState(order), now)) ==
        Right(Seq:
          order.id <-: OrderFailed(pos)))

    "Processed failed not in try/catch -> OrderFailed" in :
      val pos = Position(1)
      val order = Order(orderId, workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
      assert:
        OrderEventSource.nextStepEvents(order.id)
          .calculateEventList(EventColl(newControllerState(order), now)) ==
          Right(Seq:
            order.id <-: OrderFailed(pos))

    "Try catch and fork" in :
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
      val aChild: Order[Order.State] =
        val pos = Position(0) / BranchId.try_(0) % 0 / "fork+🥕" % 0 // Execute
        Order(OrderId("ORDER|🥕"), workflow.id /: pos, Order.Processed,
          historicOutcomes = Vector(HistoricOutcome(pos, failed7)),
          parent = Some(orderId))
      val bChild: Order[Order.State] =
        val pos = Position(0) / BranchId.try_(0) % 0 / "fork+🍋" % 1 // End
        Order(OrderId("ORDER|🍋"), workflow.id /: pos, Order.Ready(),
          historicOutcomes = Vector(HistoricOutcome(pos, failed7)),
          parent = Some(orderId))
      val forkingOrder = Order(orderId,
        workflow.id /: (Position(0) / BranchId.try_(0) % 0), // Fork
        Order.Forked(Vector(
          Order.Forked.Child("🥕", aChild.id),
          Order.Forked.Child("🍋", bChild.id))))

      val controllerState = ControllerState.forTest(
        orders = Seq(forkingOrder, aChild, bChild),
        workflows = Seq(workflow))

      def orderFailedInFork(branchId: String) = OrderFailedInFork(
        Position(0) / BranchId.try_(0) % 0 / BranchId.fork(branchId) % 0)

      var coll = locally:
        for
          coll <- Right(EventColl(controllerState, now))
          coll <- coll:
            OrderEventSource.fail(aChild.id)
          coll <- coll:
            OrderEventSource.fail(bChild.id)
        yield coll
      .orThrow
      assert(coll.keyedEventList == Seq(
        aChild.id <-: OrderFailedInFork(aChild.position),
        bChild.id <-: OrderFailedInFork(bChild.position)))

      val nextEventCalc: EventCalc[ControllerState, OrderEvent | PlanFinishedEvent] =
        (OrderEventSource.nextStepEvents(aChild.id) |+| OrderEventSource.nextStepEvents(bChild.id)).widen

      // TODO Is FailedInFork replaceable by Ready and !lastOutcome.isSucceeded?

      val orderJoined = forkingOrder.id <-:
        OrderJoined(OrderOutcome.Failed(Some(
          """Order:ORDER|🥕 Failed;
            |Order:ORDER|🍋 Failed"""
            .stripMargin)))
      assert:
        (OrderEventSource.nextStepEvents(aChild.id) |+| OrderEventSource.nextStepEvents(bChild.id))
          .widen.calculateEventList(coll) == Right(List(orderJoined))

      assert:
        OrderEventSource.nextStepEvents(aChild.id).widen.calculateEventList(coll) ==
          Right(Seq:
            orderJoined)
      assert:
        OrderEventSource.nextStepEvents(bChild.id).widen.calculateEventList(coll) ==
          Right(Seq:
            orderJoined)
      assert:
        OrderEventSource.nextStepEvents(forkingOrder.id).widen.calculateEventList(coll) ==
          Right(Seq:
            orderJoined)
  }


object OrderEventSourceTest:

  private val now = ts"2026-01-21T12:00:00Z"
  private val ForkWorkflow = ForkTestSetting.TestWorkflow
  private val TestAgentPath = AgentPath("AGENT")
  private val subagentId = SubagentId("SUBAGENT")
  private val succeededOrderId = OrderId("SUCCESS")
  private val succeededOrder = Order(succeededOrderId, ForkWorkflow.id /: Position(0), Order.Processed,
    historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Succeeded(NamedValues.rc(0)))))
  private val failedOrder = Order(OrderId("FAILED"), ForkWorkflow.id /: Position(0), Order.Processed,
    historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Failed(NamedValues.rc(1)))))
  private val orderForked = OrderForked(Vector(
    OrderForked.Child("🥕", OrderId("ORDER|🥕")),
    OrderForked.Child("🍋", OrderId("ORDER|🍋"))))

  private val executeScript = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("executable")))

  private def step(workflow: Workflow, outcome: OrderOutcome): Seq[OrderEvent | PlanFinishedEvent] =
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

    def step(): Seq[OrderEvent | PlanFinishedEvent] =
      process.step(orderId).map(_.event)

    def update(event: OrderEvent) =
      process.update(orderId <-: event)

  final class Process(workflow: Workflow):
    val idToWorkflow = Map(workflow.id -> workflow)
    val idToOrder = mutable.Map[OrderId, Order[Order.State]]()
    private val eventHandler = new OrderEventHandler(idToWorkflow.checked)
    private val inProcess = mutable.Set.empty[OrderId]

    private def newEngineState(isAgent: Boolean): EngineState_[?] =
      if isAgent then
        AgentState.forTest(orders = idToOrder.values, workflows = idToWorkflow.values)
      else
        ControllerState.forTest(orders = idToOrder.values, workflows = idToWorkflow.values)

    def jobStep(orderId: OrderId, outcome: OrderOutcome = OrderOutcome.succeeded): Unit =
      update(orderId <-: OrderProcessingStarted(subagentId))
      update(orderId <-: OrderProcessed(outcome))

    def run(orderId: OrderId): List[KeyedEvent[OrderEvent | PlanFinishedEvent]] =
      step(orderId) match
        case keyedEvents if keyedEvents.nonEmpty =>
          keyedEvents ::: (if idToOrder contains orderId then run(orderId) else Nil)
        case _ => Nil

    def step(orderId: OrderId): List[KeyedEvent[OrderEvent | PlanFinishedEvent]] =
      val keyedEvents = nextEvents(orderId)
      keyedEvents foreach update
      keyedEvents

    private def nextEvents(orderId: OrderId)
    : List[KeyedEvent[OrderEvent | PlanFinishedEvent]] =
      val order = idToOrder(orderId)
      if order.detaching.isRight then
        List(order.id <-: OrderDetached)
      else
        (order.state, workflow.instruction(order.position)) match
          case (_: Order.Ready, _: Execute) =>
            if order.isDetached then
              List(order.id <-: OrderAttachable(TestAgentPath))
            else if order.isAttaching then
              List(order.id <-: OrderAttached(TestAgentPath))
            else
              List(order.id <-: OrderProcessingStarted(subagentId))

          case _ if inProcess contains orderId =>
            List(orderId <-: OrderProcessed(OrderOutcome.succeededRC0))

          case _ =>
            OrderEventSource.nextStepEvents(orderId)
              .untypedCalculateEventList(newEngineState(isAgent = order.isAttached), TimeCtx(now))
              .orThrow

    def update(keyedEvent: KeyedEvent[OrderEvent | PlanFinishedEvent]): Unit =
      keyedEvent match
        case KeyedEvent(orderId: OrderId, event: OrderAdded) =>
          idToOrder.insert(orderId, Order.fromOrderAdded(orderId, event))

        case KeyedEvent(orderId: OrderId, event: OrderCoreEvent) =>
          processEvent(keyedEvent)
          if !event.isInstanceOf[OrderFinished] then
            idToOrder(orderId) = idToOrder(orderId).applyEvent(event).orThrow

        case _ =>
          sys.error(s"Unhandled: $keyedEvent")

    private def processEvent(keyedEvent: KeyedEvent[OrderEvent | PlanFinishedEvent])
    : Unit =
      keyedEvent match
        case KeyedEvent(orderId: OrderId, OrderProcessingStarted(_, _, _, _)) =>
          inProcess += orderId

        case KeyedEvent(orderId: OrderId, _: OrderProcessed) =>
          inProcess -= orderId

        case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
          eventHandler
            .handleEvent(idToOrder(orderId), event)
            .orThrow
            .foreach:
              case FollowUp.AddChild(derivedOrder) =>
                idToOrder.insert(derivedOrder.id, derivedOrder)

              case FollowUp.Delete(deleteOrderId) =>
                idToOrder -= deleteOrderId

              case o => sys.error(s"$keyedEvent ~~> $o")
