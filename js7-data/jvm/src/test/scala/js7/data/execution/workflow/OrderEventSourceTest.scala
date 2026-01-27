//package js7.data.execution.workflow
//
//import cats.syntax.option.*
//import js7.base.problem.Checked
//import js7.base.problem.Checked.Ops
//import js7.base.test.OurTestSuite
//import js7.base.time.TimestampForTests.ts
//import js7.base.time.WallClock
//import js7.base.utils.Collections.implicits.*
//import js7.base.utils.ScalaUtils.syntax.*
//import js7.data.Problems.CancelStartedOrderProblem
//import js7.data.agent.AgentPath
//import js7.data.board.NoticeEvent.NoticeDeleted
//import js7.data.command.CancellationMode.FreshOrStarted
//import js7.data.command.{CancellationMode, SuspensionMode}
//import js7.data.controller.ControllerState
//import js7.data.event.{Event, EventCalc, EventColl, KeyedEvent}
//import js7.data.execution.workflow.OrderEventSourceTest.*
//import js7.data.job.{PathExecutable, ShellScriptExecutable}
//import js7.data.lock.{Lock, LockPath, LockState}
//import js7.data.order.OrderEvent.OrderResumed.ReplaceHistoricOutcome
//import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancellationMarked, OrderCancelled, OrderCaught, OrderCoreEvent, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderLocksReleased, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderResumed, OrderResumptionMarked, OrderStarted, OrderSuspended, OrderSuspensionMarked}
//import js7.data.order.{HistoricOutcome, Order, OrderEvent, OrderId, OrderMark, OrderOutcome}
//import js7.data.plan.PlanEvent.{PlanDeleted, PlanFinished}
//import js7.data.plan.PlanFinishedEvent
//import js7.data.problems.{CannotResumeOrderProblem, CannotSuspendOrderProblem, UnreachableOrderPositionProblem}
//import js7.data.state.OrderEventHandler.FollowUp
//import js7.data.state.{AgentTestState, ControllerTestState, EngineState, EngineState_, OrderEventHandler, TestEngineState}
//import js7.data.subagent.Problems.ProcessLostDueToRestartProblem
//import js7.data.subagent.SubagentId
//import js7.data.value.NamedValues
//import js7.data.value.expression.Expression.{BooleanConstant, Equal, LastReturnCode, NumericConstant}
//import js7.data.workflow.instructions.executable.WorkflowJob
//import js7.data.workflow.instructions.{Execute, Fail, Fork, ForkBranchId, If, LockInstruction, TryInstruction}
//import js7.data.workflow.position.BranchId.{Else, Then, catch_, try_}
//import js7.data.workflow.position.BranchPath.syntax.*
//import js7.data.workflow.position.{BranchId, Position}
//import js7.data.workflow.test.ForkTestSetting
//import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath}
//import scala.collection.{IndexedSeqView, mutable}
//
///**
//  * @author Joacim Zschimmer
//  */
//final class OrderEventSourceTest extends OurTestSuite:
//
//  "ProcessLost" - {
//    val rawOrder = Order(OrderId("PROCESS-LOST"), TestWorkflowId /: Position(2),
//      Order.Processed,
//      historicOutcomes = Vector(
//        HistoricOutcome(Position(0), OrderOutcome.processLost(ProcessLostDueToRestartProblem))))
//
//    def testX[S <: EngineState_[S]](order: Order[Order.State], engineState: S) =
//      val now = ts"2026-01-21T12:00:00Z"
//      assert(OrderEventSource.nextEvents(order.id, engineState, now) ==
//        List:
//          order.id <-: OrderMoved(order.position))  // Move to same InstructionNr to repeat the job
//
//    for isAgent <- Seq(false, true) do s"isAgent=$isAgent" in:
//      if isAgent then
//        val order = rawOrder.copy(
//          attachedState = Some(Order.Attached(agentPath = TestAgentPath)))
//        testX(
//          order,
//          AgentTestState.of(
//            orders = Some(Seq(order)),
//            workflows = Some(Seq(ForkWorkflow))))
//      else
//        val order = rawOrder
//        testX(
//          order,
//          ControllerTestState.of(
//            orders = Some(Seq(order)),
//            workflows = Some(Seq(ForkWorkflow))))
//  }
//
//  "if" - {
//    val workflow = Workflow.of(TestWorkflowId,
//      executeScript,                                  // 0
//      If(Equal(LastReturnCode, NumericConstant(0))):  // 1
//        Workflow.of(executeScript),                   // 1/then:0
//      executeScript)                                  // 2
//
//    "then branch executed" in:
//      assert(step(workflow, OrderOutcome.Succeeded(NamedValues.rc(0))) == Seq(OrderMoved(Position(1) / Then % 0)))
//
//    "then branch skipped" in:
//      assert(step(workflow, OrderOutcome.Succeeded(NamedValues.rc(1))) == Seq(OrderMoved(Position(2))))
//
//    "again, all events" in:
//      val process = new SingleOrderProcess(workflow)
//      process.update(OrderAdded(TestWorkflowId))
//      process.transferToAgent(TestAgentPath)
//      process.update(OrderStarted)
//      process.jobStep()
//      assert(process.step() == Seq(OrderMoved(Position(1) / Then % 0)))
//      process.jobStep()
//      assert(process.step() == Seq(OrderMoved(Position(2))))
//      process.jobStep()
//      assert(process.step() == Seq(OrderMoved(Position(3))))
//      process.transferToController()
//      assert(process.step() == Seq(OrderFinished()))
//
//    "then branch not executed" in:
//      assert(step(workflow, OrderOutcome.Succeeded(NamedValues.rc(1))) == Seq(OrderMoved(Position(2))))
//  }
//
//  "if returnCode else" - {
//    val workflow = Workflow.of(TestWorkflowId,
//      executeScript,                                  // 0
//      If(Equal(LastReturnCode, NumericConstant(0)))   // 1
//        .Then:
//          Workflow.of(executeScript)    // 1/0:0
//        .Else:
//          Workflow.of(executeScript),   // 1/1:0
//      executeScript)                                  // 2
//
//    "then branch executed" in:
//      assert(step(workflow, OrderOutcome.succeededRC0) == Seq(OrderMoved(Position(1) / Then % 0)))
//
//    "else branch executed" in:
//      assert(step(workflow, OrderOutcome.Succeeded(NamedValues.rc(1))) == Seq(OrderMoved(Position(1) / Else % 0)))
//  }
//
//  "fork" in:
//    val process = new Process(ForkWorkflow)
//    val orderId = succeededOrderId
//
//    process.update(orderId <-: OrderAdded(TestWorkflowId))
//    process.update(orderId <-: OrderAttachable(TestAgentPath))
//    process.update(orderId <-: OrderAttached(TestAgentPath))
//    assert(process.run(orderId) == List(
//      orderId <-: OrderDetachable,
//      orderId <-: OrderDetached,
//      orderId <-: OrderStarted,
//      orderId <-: OrderForked(Vector(
//        OrderForked.Child("🥕", orderId / "🥕"),
//        OrderForked.Child("🍋", orderId / "🍋")))))
//
//    assert(process.run(orderId / "🥕") == List(
//      orderId / "🥕" <-: OrderAttachable(TestAgentPath),
//      orderId / "🥕" <-: OrderAttached(TestAgentPath),
//      orderId / "🥕" <-: OrderProcessingStarted(subagentId),
//      orderId / "🥕" <-: OrderProcessed(OrderOutcome.succeededRC0),
//      orderId / "🥕" <-: OrderMoved(Position(0) / "fork+🥕" % 1),
//      orderId / "🥕" <-: OrderDetachable,
//      orderId / "🥕" <-: OrderDetached))
//
//    assert(process.step(orderId).isEmpty)  // Nothing to join
//
//    assert(process.run(orderId / "🍋") == List(
//      orderId / "🍋" <-: OrderAttachable(TestAgentPath),
//      orderId / "🍋" <-: OrderAttached(TestAgentPath),
//      orderId / "🍋" <-: OrderProcessingStarted(subagentId),
//      orderId / "🍋" <-: OrderProcessed(OrderOutcome.succeededRC0),
//      orderId / "🍋" <-: OrderMoved(Position(0) / "fork+🍋" % 1),
//      orderId / "🍋" <-: OrderDetachable,
//      orderId / "🍋" <-: OrderDetached,
//      orderId <-: OrderJoined(OrderOutcome.succeeded)))
//    assert(process.step(orderId) == Seq(orderId <-: OrderMoved(Position(1))))
//
//    assert(process.step(orderId) == Seq(orderId <-: OrderForked(Vector(
//      OrderForked.Child("🥕", orderId / "🥕"),
//      OrderForked.Child("🍋", orderId / "🍋")))))
//
//    assert(process.run(orderId / "🥕") == List(
//      orderId / "🥕" <-: OrderAttachable(TestAgentPath),
//      orderId / "🥕" <-: OrderAttached(TestAgentPath),
//      orderId / "🥕" <-: OrderProcessingStarted(subagentId),
//      orderId / "🥕" <-: OrderProcessed(OrderOutcome.succeededRC0),
//      orderId / "🥕" <-: OrderMoved(Position(1) / "fork+🥕" % 1),
//      orderId / "🥕" <-: OrderDetachable,
//      orderId / "🥕" <-: OrderDetached))
//
//    assert(process.step(orderId).isEmpty)  // Nothing to join
//
//    assert(process.run(orderId / "🍋") == List(
//      orderId / "🍋" <-: OrderAttachable(TestAgentPath),
//      orderId / "🍋" <-: OrderAttached(TestAgentPath),
//      orderId / "🍋" <-: OrderProcessingStarted(subagentId),
//      orderId / "🍋" <-: OrderProcessed(OrderOutcome.succeededRC0),
//      orderId / "🍋" <-: OrderMoved(Position(1) / "fork+🍋" % 1),
//      orderId / "🍋" <-: OrderDetachable,
//      orderId / "🍋" <-: OrderDetached,
//      orderId <-: OrderJoined(OrderOutcome.succeeded)))
//
//    assert(process.step(orderId) == Seq(orderId <-: OrderMoved(Position(2))))
//    assert(process.step(orderId) == Seq(orderId <-: OrderAttachable(TestAgentPath)))
//    assert(process.step(orderId) == Seq(orderId <-: OrderAttached(TestAgentPath)))
//    assert(process.step(orderId) == Seq(orderId <-: OrderProcessingStarted(subagentId)))
//    // and so forth...
//
//  //"applyMoveInstructions" - {
//  //  for isAgent <- Seq(false, true) do s"isAgent=$isAgent" - {
//  //    "Job, Fork" in:
//  //      val engineState = newEngineState(ForkWorkflow, List(succeededOrder, failedOrder), isAgent = isAgent)
//  //      assert(EventSource.applyMoveInstructions(succeededOrder.withPosition(Position(0))) == Right(Vector.empty))
//  //      assert(eventSource.applyMoveInstructions(succeededOrder.withPosition(Position(1))) == Right(Vector.empty))
//  //
//  //    "In forked order" in:
//  //      val engineState = newEngineState(ForkWorkflow, List(succeededOrder, failedOrder), isAgent = isAgent)
//  //      assert(EventSource.applyMoveInstructions(succeededOrder.withPosition(Position(1) / "fork+🥕" % 1)) == Right(Vector.empty))
//  //  }
//  //}
//
//  "cancel, suspend, resume, false" - {
//    val detached = none[Order.AttachedState]
//    val attaching = Some(Order.Attaching(TestAgentPath))
//    val attached = Some(Order.Attached(TestAgentPath))
//    val detaching = Some(Order.Detaching(TestAgentPath))
//
//    val historicOps = Seq(OrderResumed.ReplaceHistoricOutcome(Position(0), OrderOutcome.failed))
//
//    "Order.mark.isEmpty" - {
//      val unmarkedOrder = Order(OrderId("ORDER"), TestWorkflowId /: Position(0), Order.Fresh())
//
//      "Fresh" - {
//        val freshOrder = unmarkedOrder
//
//        "Detached" in:
//          testController(freshOrder, detached): (order, controllerState) =>
//            assert(OrderEventSource.nextEvents(order.id, controllerState, now) == Right(Seq(
//              order.id <-: OrderStarted,
//              order.id <-: OrderForked(Vector(
//                OrderForked.Child(ForkBranchId("🥕"), order.id / "🥕"),
//                OrderForked.Child(ForkBranchId("🍋"), order.id / "🍋"))))))
//
//          testController(freshOrder, detached): (order, controllerState) =>
//            assert(OrderEventSource.cancel(order.id, CancellationMode.FreshOnly).calculateEvents(controllerState, now) == Right(Seq(OrderCancelled)))
//
//          def toEvents[S <: EngineState_[S]](
//            eventCalc: EventCalc[S, OrderCoreEvent],
//            controllerState: S)
//          : Checked[Vector[KeyedEvent[OrderCoreEvent]]] =
//            eventCalc.calculateEvents(controllerState, now).map(_.toVector)
//
//          assert:
//            testController2(freshOrder, detached):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancelled))
//
//          assert:
//            testController2(freshOrder, detached):
//              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
//            == Right(Seq(OrderSuspended))
//
//          assert:
//            testController2(freshOrder, detached):
//              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//
//        "Attaching" in:
//          assert:
//            testController2(freshOrder, attaching):
//              OrderEventSource.nextEvents(freshOrder.id)
//            == Right(Nil)
//          assert:
//            testAgent2(freshOrder, attaching):
//              OrderEventSource.nextEvents(freshOrder.id)
//            == Right(Nil)
//
//          assert:
//            testController2(freshOrder, attaching):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))
//          assert:
//            testAgent2(freshOrder, attaching):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly)
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))
//
//          assert:
//            testController2(freshOrder, attaching):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//          assert:
//            testAgent2(freshOrder, attaching):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//
//          assert:
//            testController2(freshOrder, attaching):
//              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
//            == Right(Seq(OrderSuspensionMarked()))
//          assert:
//            testAgent2(freshOrder, attaching):
//              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
//            == Right(Seq(OrderSuspensionMarked()))
//
//          assert:
//            testController2(freshOrder, attaching):
//              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//          assert:
//            testAgent2(freshOrder, attaching):
//              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//
//        "Attached" in:
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.nextEvents(freshOrder.id)
//            == Right(Nil)
//          assert:
//            testAgent2(freshOrder, attached):
//              OrderEventSource.nextEvents(freshOrder.id)
//            == Seq(freshOrder.id <-: OrderDetachable)
//
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly       )
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))
//          assert:
//            testAgent2(freshOrder, attached):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly       )
//            == Right(Seq(OrderDetachable))
//
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//          assert:
//            testAgent2(freshOrder, attached):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderDetachable))
//
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
//            == Right(Seq(OrderSuspensionMarked()))
//          assert:
//            testAgent2(freshOrder, attached):
//              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
//            == Right(Seq(OrderDetachable))
//
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//          assert:
//            testAgent2(freshOrder, attached):
//              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//
//        "Detaching" in:
//          assert:
//            testController2(freshOrder, detaching):
//              OrderEventSource.nextEvents(freshOrder.id)
//            == Right(Nil)
//          assert:
//            testAgent2(freshOrder, detaching):
//              OrderEventSource.nextEvents(freshOrder.id)
//            == Right(Nil)
//
//          assert:
//            testController2(freshOrder, detaching):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly       )
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))
//          assert:
//            testAgent2(freshOrder, detaching):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly       )
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))
//
//          assert:
//            testController2(freshOrder, detaching):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//          assert:
//            testAgent2(freshOrder, detaching):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//
//          assert:
//            testController2(freshOrder, detaching):
//              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
//            == Right(Seq(OrderSuspensionMarked()))
//          assert:
//            testAgent2(freshOrder, detaching):
//              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
//            == Right(Seq(OrderSuspensionMarked()))
//
//          assert:
//            testController2(freshOrder, detaching):
//              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//          assert:
//            testAgent2(freshOrder, detaching):
//              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//      }
//
//      "Ready" - {
//        val readyOrder = unmarkedOrder.copy(state = Order.Ready())
//
//        "Detached" in:
//          assert:
//            testController2(readyOrder, attachedState = detached):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Seq(readyOrder.id <-: orderForked)
//
//          assert:
//            testController2(readyOrder, attachedState = detached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//
//          assert:
//            testController2(readyOrder, attachedState = detached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancelled))
//
//        "Attaching" in:
//          assert:
//            testController2(readyOrder, attaching):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Right(Nil)
//          assert:
//            testAgent2(readyOrder, attaching):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Right(Nil)
//
//          assert:
//            testController2(readyOrder, attaching):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//          assert:
//            testAgent2(readyOrder, attaching):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//
//          assert:
//            testController2(readyOrder, attaching):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//          assert:
//            testAgent2(readyOrder, attaching):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//
//          assert:
//            testController2(readyOrder, attaching):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(Seq(OrderSuspensionMarked()))
//          assert:
//            testAgent2(readyOrder, attaching):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(Seq(OrderSuspensionMarked()))
//
//          assert:
//            testController2(readyOrder, attaching):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//          assert:
//            testAgent2(readyOrder, attaching):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//
//        "Attached" in:
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Right(Nil)
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Seq(readyOrder.id <-: OrderDetachable)
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderDetachable))
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(Seq(OrderSuspensionMarked()))
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(Seq(OrderDetachable))
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//
//        "Detaching" in:
//          assert:
//            testController2(readyOrder, detaching):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Right(Nil)
//          assert:
//            testAgent2(readyOrder, detaching):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Right(Nil)
//
//          assert:
//            testController2(readyOrder, detaching):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//          assert:
//            testAgent2(readyOrder, detaching):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//
//          assert:
//            testController2(readyOrder, detaching):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//          assert:
//            testAgent2(readyOrder, detaching):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//
//          assert:
//            testController2(readyOrder, detaching):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(Seq(OrderSuspensionMarked()))
//          assert:
//            testAgent2(readyOrder, detaching):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(Seq(OrderSuspensionMarked()))
//
//          assert:
//            testController2(readyOrder, detaching):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//          assert:
//            testAgent2(readyOrder, detaching):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//      }
//
//      "Processing Attached" in:
//        val processingOrder = unmarkedOrder.copy(state = Order.Processing(subagentId))
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.nextEvents(processingOrder.id)
//          == Right(Nil)
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.nextEvents(processingOrder.id)
//          == Right(Nil)
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOnly       )
//          == Left(CancelStartedOrderProblem(processingOrder.id))
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOnly       )
//          == Left(CancelStartedOrderProblem(processingOrder.id))
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOrStarted())
//          == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOrStarted())
//          == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.suspend(processingOrder.id, SuspensionMode())
//          == Right(Seq(OrderSuspensionMarked()))
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.suspend(processingOrder.id, SuspensionMode())
//          == Right(Seq(OrderSuspensionMarked()))
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.resume(processingOrder.id, None, Nil, false, None)
//          == Left(CannotResumeOrderProblem)
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.resume(processingOrder.id, None, Nil, false, None)
//          == Left(CannotResumeOrderProblem)
//    }
//
//    "OrderMark.Cancelling(FreshOnly)" - {
//      val cancellingOrder = Order(OrderId("ORDER"), TestWorkflowId /: Position(0), Order.Fresh(),
//        mark = Some(OrderMark.Cancelling(CancellationMode.FreshOnly)))
//
//      "Fresh" - {
//        val freshOrder = cancellingOrder
//
//        "Detached" in:
//          assert:
//            testController2(freshOrder, detached):
//              OrderEventSource.nextEvents(freshOrder.id)
//            == Seq(freshOrder.id <-: OrderCancelled)
//
//        "Attached" in:
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.nextEvents(freshOrder.id)
//            == Right(Nil)
//          assert:
//            testAgent2(freshOrder, attached):
//              OrderEventSource.nextEvents(freshOrder.id)
//            == Seq(freshOrder.id <-: OrderDetachable)
//
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly       )
//            == Right(Nil)
//          assert:
//            testAgent2(freshOrder, attached):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly       )
//            == Right(Seq(OrderDetachable))
//
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(FreshOrStarted(None))))
//          assert:
//            testAgent2(freshOrder, attached):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderDetachable))
//
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
//            == Left(CannotSuspendOrderProblem)
//          assert:
//            testAgent2(freshOrder, attached):
//              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
//            == Left(CannotSuspendOrderProblem)
//
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//          assert:
//            testAgent2(freshOrder, attached):
//              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//          assert:
//            testAgent2(freshOrder, attached):
//              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//      }
//    }
//
//    "OrderMark.Cancelling(FreshOrStarted)" - {
//      val cancellingOrder = Order(OrderId("ORDER"), TestWorkflowId /: Position(0), Order.Fresh(),
//        mark = Some(OrderMark.Cancelling(CancellationMode.FreshOnly)))
//
//      "Ready" - {
//        val readyOrder = cancellingOrder.copy(state = Order.Ready())
//
//        "Detached" in:
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Seq(readyOrder.id <-: orderForked)
//
//        "Attached" in:
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Right(Nil)
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Seq(readyOrder.id <-: OrderDetachable)
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(FreshOrStarted(None))))
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderDetachable))
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Left(CannotSuspendOrderProblem)
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Left(CannotSuspendOrderProblem)
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//      }
//
//      "Processing Attached" in:
//        val processingOrder = cancellingOrder.copy(state = Order.Processing(subagentId))
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.nextEvents(processingOrder.id)
//          == Right(Nil)
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.nextEvents(processingOrder.id)
//          == Right(Nil)
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOnly       )
//          == Left(CancelStartedOrderProblem(processingOrder.id))
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOnly       )
//          == Left(CancelStartedOrderProblem(processingOrder.id))
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOrStarted())
//          == Right(Seq(OrderCancellationMarked(FreshOrStarted(None))))
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOrStarted())
//          == Right(Seq(OrderCancellationMarked(FreshOrStarted(None))))
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.suspend(processingOrder.id, SuspensionMode())
//          == Left(CannotSuspendOrderProblem)
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.suspend(processingOrder.id, SuspensionMode())
//          == Left(CannotSuspendOrderProblem)
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.resume(processingOrder.id, None, Nil, false, None)
//          == Left(CannotResumeOrderProblem)
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.resume(processingOrder.id, None, Nil, false, None)
//          == Left(CannotResumeOrderProblem)
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.resume(processingOrder.id, Some(Position(1)), Nil, false, None)
//          == Left(CannotResumeOrderProblem)
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.resume(processingOrder.id, Some(Position(1)), Nil, false, None)
//          == Left(CannotResumeOrderProblem)
//    }
//
//    "OrderMark.Suspending" - {
//      val suspendingOrder = Order(OrderId("ORDER"), TestWorkflowId /: Position(0), Order.Ready(), mark = Some(OrderMark.Suspending()))
//
//      "Ready" - {
//        val readyOrder = suspendingOrder
//
//        "Detached" in:
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Seq(readyOrder.id <-: OrderSuspended)
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancelled))
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(OrderSuspended :: Nil)
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumptionMarked()))
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, Some(false))
//            == Right(Seq(OrderResumptionMarked(restartKilledJob = false)))
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.resume(readyOrder.id, None, Nil, true, Some(false))
//            == Right(Seq(OrderResumptionMarked(asSucceeded = true)))
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, Some(true))
//            == Right(Seq(OrderResumptionMarked(restartKilledJob = true)))
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Seq(ReplaceHistoricOutcome(Position(0), OrderOutcome.succeeded)), false, None)
//            == Left(CannotResumeOrderProblem)
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.resume(readyOrder.id, None, Seq(ReplaceHistoricOutcome(Position(0), OrderOutcome.succeeded)), false, None)
//            == Left(CannotResumeOrderProblem)
//
//        "Attached" in:
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Right(Nil)
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Seq(readyOrder.id <-: OrderDetachable)
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderDetachable))
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(Nil)
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(OrderDetachable :: Nil)
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumptionMarked()))
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumptionMarked()))
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, None, historicOps, false, None)
//            == Left(CannotResumeOrderProblem)
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, None, historicOps, false, None)
//            == Left(CannotResumeOrderProblem)
//      }
//
//      "Processing Attached" in:
//        val processingOrder = suspendingOrder.copy(state = Order.Processing(subagentId))
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.nextEvents(processingOrder.id)
//          == Right(Nil)
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.nextEvents(processingOrder.id)
//          == Right(Nil)
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOnly       )
//          == Left(CancelStartedOrderProblem(processingOrder.id))
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOrStarted())
//          == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.suspend(processingOrder.id, SuspensionMode())
//          == Right(Nil)
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.suspend(processingOrder.id, SuspensionMode())
//          == Right(Nil)
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.resume(processingOrder.id, None, Nil, false, None)
//          == Right(Seq(OrderResumptionMarked()))
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.resume(processingOrder.id, None, Nil, false, None)
//          == Right(Seq(OrderResumptionMarked()))
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.resume(processingOrder.id, Some(Position(1)), Nil, false, None)
//          == Left(CannotResumeOrderProblem)
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.resume(processingOrder.id, Some(Position(1)), Nil, false, None)
//          == Left(CannotResumeOrderProblem)
//
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.resume(processingOrder.id, None, historicOps, false, None)
//          == Left(CannotResumeOrderProblem)
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.resume(processingOrder.id, None, historicOps, false, None)
//          == Left(CannotResumeOrderProblem)
//    }
//
//    "OrderMark.Resuming isSuspended" - {
//      val resumingOrder = Order(OrderId("ORDER"),
//        TestWorkflowId /: Position(0),
//        Order.Ready(),
//        mark = Some(OrderMark.Resuming()),
//        isSuspended = true)
//
//      "Ready" - {
//        val readyOrder = resumingOrder
//
//        "Detached" in:
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Seq(readyOrder.id <-: OrderResumed())
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly)
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancelled))
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(Seq(OrderSuspended))
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumed())/*should already be happened*/)
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//
//        "Attached" in:
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Right(Nil)
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Seq(readyOrder.id <-: OrderResumed())
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderDetachable))
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(Seq(OrderSuspensionMarked(SuspensionMode())))
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(Seq(OrderDetachable))
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Right(Nil)
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Right(OrderResumed() :: Nil)
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
//            == Left(CannotResumeOrderProblem)
//      }
//    }
//
//    "Order.isSuspended" - {
//      val suspendedOrder = Order(OrderId("ORDER"), TestWorkflowId /: Position(0), Order.Fresh(), isSuspended = true)
//
//      "Fresh" - {
//        val freshOrder = suspendedOrder
//
//        "Detached" in:
//          assert:
//            testController2(freshOrder, detached):
//              OrderEventSource.nextEvents(freshOrder.id)
//            == Right(Nil)
//
//          assert:
//            testController2(freshOrder, detached):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly       )
//            == Right(Seq(OrderCancelled))
//
//          assert:
//            testController2(freshOrder, detached):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancelled))
//
//          assert:
//            testController2(freshOrder, detached):
//              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
//            == Right(Nil)
//
//          assert:
//            testController2(freshOrder, detached):
//              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumed()))
//
//          assert:
//            testController2(freshOrder, detached):
//              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
//            == Right(Seq(OrderResumed(Some(Position(1)))))
//
//          assert:
//            testController2(freshOrder, detached):
//              OrderEventSource.resume(freshOrder.id, Some(Position(99)), Nil, false, None)
//            == Left(UnreachableOrderPositionProblem)
//
//        "Attaching" in:
//          assert:
//            testController2(freshOrder, attaching):
//              OrderEventSource.nextEvents(freshOrder.id)
//            == Right(Nil)
//          assert:
//            testAgent2(freshOrder, attaching):
//              OrderEventSource.nextEvents(freshOrder.id)
//            == Right(Nil)
//
//          assert:
//            testController2(freshOrder, attaching):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly       )
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))
//          assert:
//            testAgent2(freshOrder, attaching):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly       )
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))
//
//          assert:
//            testController2(freshOrder, attaching):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//          assert:
//            testAgent2(freshOrder, attaching):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//
//          assert:
//            testController2(freshOrder, attaching):
//              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
//            == Right(Nil)
//          assert:
//            testAgent2(freshOrder, attaching):
//              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
//            == Right(Nil)
//
//          assert:
//            testController2(freshOrder, attaching):
//              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumptionMarked()))
//          assert:
//            testAgent2(freshOrder, attaching):
//              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumptionMarked()))
//
//          assert:
//            testController2(freshOrder, attaching):
//              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
//            == Right(Seq(OrderResumptionMarked(Some(Position(1)))))
//          assert:
//            testAgent2(freshOrder, attaching):
//              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
//            == Right(Seq(OrderResumptionMarked(Some(Position(1)))))
//
//          assert:
//            testController2(freshOrder, attaching):
//              OrderEventSource.resume(freshOrder.id, Some(Position(1)), historicOps, false, None)
//            == Right(Seq(OrderResumptionMarked(Some(Position(1)), historicOps)))
//          assert:
//            testAgent2(freshOrder, attaching):
//              OrderEventSource.resume(freshOrder.id, Some(Position(1)), historicOps, false, None)
//            == Right(Seq(OrderResumptionMarked(Some(Position(1)), historicOps)))
//
//        "Attached" in:
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.nextEvents(freshOrder.id)
//            == Right(Nil)
//          assert:
//            testAgent2(freshOrder, attached):
//              OrderEventSource.nextEvents(freshOrder.id)
//            == Right(Nil)
//
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly       )
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))
//          assert:
//            testAgent2(freshOrder, attached):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly       )
//            == Right(Seq(OrderDetachable))
//
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//          assert:
//            testAgent2(freshOrder, attached):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderDetachable))
//
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
//            == Right(Nil)
//          //assert:
//          //  testAgent(freshOrder, attached):
//          //    OrderEventSource.suspend(freshOrder.id, SuspensionMode())
//          //  == Right(Seq(OrderDetachable)))
//
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumptionMarked()))
//          assert:
//            testAgent2(freshOrder, attached):
//              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumed()))
//
//          assert:
//            testController2(freshOrder, attached):
//              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
//            == Right(Seq(OrderResumptionMarked(Some(Position(1)))))
//          assert:
//            testAgent2(freshOrder, attached):
//              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
//            == Right(Seq(OrderResumed(Some(Position(1)))))
//
//        "Detaching" in:
//          assert:
//            testController2(freshOrder, detaching):
//              OrderEventSource.nextEvents(freshOrder.id)
//            == Right(Nil)
//          assert:
//            testAgent2(freshOrder, detaching):
//              OrderEventSource.nextEvents(freshOrder.id)
//            == Right(Nil)
//
//          assert:
//            testController2(freshOrder, detaching):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly       )
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))
//          assert:
//            testAgent2(freshOrder, detaching):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOnly       )
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))
//
//          assert:
//            testController2(freshOrder, detaching):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//          assert:
//            testAgent2(freshOrder, detaching):
//              OrderEventSource.cancel(freshOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//
//          assert:
//            testController2(freshOrder, detaching):
//              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
//            == Right(Nil)
//          assert:
//            testAgent2(freshOrder, detaching):
//              OrderEventSource.suspend(freshOrder.id, SuspensionMode())
//            == Right(Nil)
//
//          assert:
//            testController2(freshOrder, detaching):
//              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumptionMarked()))
//          assert:
//            testAgent2(freshOrder, detaching):
//              OrderEventSource.resume(freshOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumptionMarked()))
//
//          assert:
//            testController2(freshOrder, detaching):
//              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
//            == Right(Seq(OrderResumptionMarked(Some(Position(1)))))
//          assert:
//            testAgent2(freshOrder, detaching):
//              OrderEventSource.resume(freshOrder.id, Some(Position(1)), Nil, false, None)
//            == Right(Seq(OrderResumptionMarked(Some(Position(1)))))
//      }
//
//      "Ready" - {
//        val readyOrder = suspendedOrder.copy(state = Order.Ready())
//
//        "Detached" in:
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Right(Nil)
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly     )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancelled))
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(Nil)
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumed()))
//
//          assert:
//            testController2(readyOrder, detached):
//              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
//            == Right(Seq(OrderResumed(Some(Position(1)))))
//
//        "Attaching" in:
//          assert:
//            testController2(readyOrder, attaching):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Right(Nil)
//          assert:
//            testAgent2(readyOrder, attaching):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Right(Nil)
//
//          assert:
//            testController2(readyOrder, attaching):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//          assert:
//            testAgent2(readyOrder, attaching):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//
//          assert:
//            testController2(readyOrder, attaching):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//          assert:
//            testAgent2(readyOrder, attaching):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//
//          assert:
//            testController2(readyOrder, attaching):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(Nil)
//          assert:
//            testAgent2(readyOrder, attaching):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(Nil)
//
//          assert:
//            testController2(readyOrder, attaching):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumptionMarked()))
//          assert:
//            testAgent2(readyOrder, attaching):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumptionMarked()))
//
//          assert:
//            testController2(readyOrder, attaching):
//              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
//            == Right(Seq(OrderResumptionMarked(Some(Position(1)))))
//          assert:
//            testAgent2(readyOrder, attaching):
//              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
//            == Right(Seq(OrderResumptionMarked(Some(Position(1)))))
//
//        "Attached" in:
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Right(Nil)
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Right(Nil)
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderDetachable))
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumptionMarked()))
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumed()))
//
//          assert:
//            testController2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
//            == Right(Seq(OrderResumptionMarked(Some(Position(1)))))
//          assert:
//            testAgent2(readyOrder, attached):
//              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
//            == Right(Seq(OrderResumed(Some(Position(1)))))
//
//        "Detaching" in:
//          assert:
//            testController2(readyOrder, detaching):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Right(Nil)
//          assert:
//            testAgent2(readyOrder, detaching):
//              OrderEventSource.nextEvents(readyOrder.id)
//            == Right(Nil)
//
//          assert:
//            testController2(readyOrder, detaching):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//          assert:
//            testAgent2(readyOrder, detaching):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOnly       )
//            == Left(CancelStartedOrderProblem(readyOrder.id))
//
//          assert:
//            testController2(readyOrder, detaching):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//          assert:
//            testAgent2(readyOrder, detaching):
//              OrderEventSource.cancel(readyOrder.id, CancellationMode.FreshOrStarted())
//            == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//
//          assert:
//            testController2(readyOrder, detaching):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(Nil)
//          assert:
//            testAgent2(readyOrder, detaching):
//              OrderEventSource.suspend(readyOrder.id, SuspensionMode())
//            == Right(Nil)
//
//          assert:
//            testController2(readyOrder, detaching):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumptionMarked()))
//          assert:
//            testAgent2(readyOrder, detaching):
//              OrderEventSource.resume(readyOrder.id, None, Nil, false, None)
//            == Right(Seq(OrderResumptionMarked()))
//
//          assert:
//            testController2(readyOrder, detaching):
//              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
//            == Right(Seq(OrderResumptionMarked(Some(Position(1)))))
//          assert:
//            testAgent2(readyOrder, detaching):
//              OrderEventSource.resume(readyOrder.id, Some(Position(1)), Nil, false, None)
//            == Right(Seq(OrderResumptionMarked(Some(Position(1)))))
//      }
//
//      "Processing Attached" in:
//        val processingOrder = suspendedOrder.copy(state = Order.Processing(subagentId))
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.nextEvents(processingOrder.id)
//          == Right(Nil)
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.nextEvents(processingOrder.id)
//          == Right(Nil)
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOnly       )
//          == Left(CancelStartedOrderProblem(processingOrder.id))
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOnly       )
//          == Left(CancelStartedOrderProblem(processingOrder.id))
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOrStarted())
//          == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.cancel(processingOrder.id, CancellationMode.FreshOrStarted())
//          == Right(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.suspend(processingOrder.id, SuspensionMode())
//          == Right(Nil)
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.suspend(processingOrder.id, SuspensionMode())
//          == Right(Nil)
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.resume(processingOrder.id, None, Nil, false, None)
//          == Right(Seq(OrderResumptionMarked()))
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.resume(processingOrder.id, None, Nil, false, None)
//          == Right(Seq(OrderResumptionMarked()))
//        assert:
//          testController2(processingOrder, attached):
//            OrderEventSource.resume(processingOrder.id, Some(Position(1)), Nil, false, None)
//          == Right(Seq(OrderResumptionMarked(Some(Position(1)))))
//        assert:
//          testAgent2(processingOrder, attached):
//            OrderEventSource.resume(processingOrder.id, Some(Position(1)), Nil, false, None)
//          == Right(Seq(OrderResumptionMarked(Some(Position(1)))))
//    }
//
//    def testController2[E <: Event](
//      templateOrder: Order[Order.State],
//      attachedState: Option[Order.AttachedState])
//      (op: EventCalc[ControllerTestState, E])
//    : Checked[Seq[KeyedEvent[E]]] =
//      testController(templateOrder, attachedState): (order, controllerState) =>
//        op.calculateEvents(controllerState, now)
//          .map(_.toVector)
//
//    def testController[R](
//      templateOrder: Order[Order.State],
//      attachedState: Option[Order.AttachedState])
//      (body: (Order[Order.State], ControllerTestState) => R)
//    : R =
//      val order = templateOrder.copy(attachedState = attachedState)
//      body(
//        order,
//        ControllerTestState.of(
//          orders = Some(Seq(order)),
//          workflows = Some(Seq(ForkWorkflow))))
//
//    def testAgent2[E <: Event](
//      templateOrder: Order[Order.State],
//      attachedState: Option[Order.AttachedState])
//      (op: EventCalc[AgentTestState, E])
//    : Checked[Seq[KeyedEvent[E]]] =
//      testAgent(templateOrder, attachedState): (order, agentState) =>
//        op.calculateEvents(agentState, now)
//          .map(_.toVector)
//
//    def testAgent[R](templateOrder: Order[Order.State], attachedState: Option[Order.AttachedState])
//      (body: (Order[Order.State], AgentTestState) => R)
//    : R =
//      val order = templateOrder.copy(attachedState = attachedState)
//      body(
//        order,
//        AgentTestState.of(
//          orders = Some(Seq(order)),
//          workflows = Some(Seq(ForkWorkflow))))
//
//    "Resume and UnreachableOrderPositionProblem" - {
//      val orderId = OrderId("SUSPENDED")
//      val lockPath = LockPath("LOCK")
//      lazy val execute = Execute.Anonymous(WorkflowJob(TestAgentPath, ShellScriptExecutable(":")))
//      lazy val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1", Vector(
//        /*0*/ execute,
//        /*1*/ If(BooleanConstant(true))
//          .Then(Workflow.of(execute))
//          .Else(Workflow.of(execute)),
//        /*2*/ TryInstruction(
//          Workflow.of(execute),
//          Workflow.of(execute)),
//        /*3*/ Fork.of(
//          "A" -> Workflow.of(execute),
//          "B" -> Workflow.of(execute)),
//        /*4*/ LockInstruction.single(lockPath, count = None, Workflow.of(execute))))
//
//      "Same level" in:
//        assert(testResume(workflow, Position(0), Position(1)) ==
//          Right(Seq(orderId <-: OrderResumed(Some(Position(1))))))
//
//      "Into and out of if-then" in:
//        assert(isResumableBackAndForth(workflow, Position(0), Position(1) / BranchId.Then % 0))
//
//      "Into and out of if-else" in:
//        assert(isResumableBackAndForth(workflow, Position(0), Position(1) / BranchId.Else % 0))
//
//      "Into and out of try" in:
//        assert(isResumableBackAndForth(workflow, Position(0), Position(2) / BranchId.Try_ % 0))
//        assert(isResumableBackAndForth(workflow, Position(0), Position(2) / BranchId.try_(3) % 0))
//
//      "Into and out of catch" in:
//        assert(isResumableBackAndForth(workflow, Position(0), Position(2) / BranchId.Catch_ % 0))
//        assert(isResumableBackAndForth(workflow, Position(0), Position(2) / BranchId.catch_(3) % 0))
//
//      "Into a fork is forbidden" in:
//        assert(testResume(workflow, Position(0), Position(3) / BranchId.fork("A") % 0) ==
//          Left(UnreachableOrderPositionProblem))
//
//      "Leaving a fork is forbidden" in:
//        assert(testResume(workflow, Position(3) / BranchId.fork("A") % 0, Position(0)) ==
//          Left(UnreachableOrderPositionProblem))
//
//      "Into a lock is forbidden" in:
//        assert(testResume(workflow, Position(0), Position(4) / BranchId.Lock % 0) ==
//          Left(UnreachableOrderPositionProblem))
//
//      "Leaving a lock is forbidden" in:
//        assert(testResume(workflow, Position(4) / BranchId.Lock % 0, Position(0)) ==
//          Left(UnreachableOrderPositionProblem))
//
//      def isResumableBackAndForth(workflow: Workflow, a: Position, b: Position) =
//        isResumable(workflow, a, b) && isResumable(workflow, b, a)
//
//      def isResumable(workflow: Workflow, from: Position, to: Position) =
//        testResume(workflow, from, to) == Right(OrderResumed(Some(to)) :: Nil)
//
//      def testResume(workflow: Workflow, from: Position, to: Position)
//      : Checked[List[KeyedEvent[OrderEvent]]] =
//        val order = Order(orderId, workflow.id /: from, Order.Ready(), isSuspended = true)
//        val controllerState = ControllerTestState.of(
//          orders = Some(Seq(order)),
//          workflows = Some(Seq(workflow)),
//          itemStates = Seq(LockState(Lock(lockPath))))
//        OrderEventSource.resume(order.id, Some(to), Nil, false, None)
//          .calculateEvents(controllerState, now)
//          .map(_.toList)
//    }
//  }
//
//  "Failed" in:
//    lazy val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1", Vector(Fail()))
//    val order = Order(OrderId("ORDER"), workflow.id /: Position(0), Order.Failed)
//    val controllerState = ControllerTestState.of(
//      orders = Some(Seq(order)),
//      workflows = Some(Seq(workflow)))
//    assert(OrderEventSource.nextEvents(order.id).calculateEvents(controllerState, now).map(_.toList) == Right(Nil))
//    assert(OrderEventSource.suspend(order.id, SuspensionMode()).calculateEvents(controllerState, now).map(_.toList) == Right(CannotSuspendOrderProblem))
//    assert(OrderEventSource.cancel(order.id, CancellationMode.Default).calculateEvents(controllerState, now).map(_.toList) ==
//      Right(OrderCancelled :: Nil))
//    assert(OrderEventSource.resume(order.id, None, Nil, false, None) ==
//      Right(OrderResumed(None, Nil) :: Nil))
//
//  "Try catch" - {
//    val workflow = WorkflowParser.parse(
//       """define workflow {
//         |  try {                                     // 0
//         |    try {                                   // 0/try:0
//         |      execute agent="a", executable="ex";   // 0/try:0/try:0
//         |    } catch {
//         |      execute agent="a", executable="ex";   // 0/try:0/catch:0
//         |    }
//         |  } catch {
//         |    execute agent="a", executable="ex";     // 0/catch:0
//         |    try (maxTries=3) {                      // 0/catch:1
//         |      execute agent="a", executable="ex";   // 0/catch:1/try:0
//         |    } catch {
//         |      retry;                                // 0/catch:1/catch:0
//         |    }
//         |  };
//         |  execute agent="a", executable="ex";       // 1
//         |}""".stripMargin).orThrow
//
//    def newControllerState(order: Order[Order.State]) =
//      ControllerTestState.of(
//        orders = Some(Seq(order)),
//        workflows = Some(Seq(workflow)))
//
//    val failed7 = OrderOutcome.Failed(NamedValues.rc(7))
//
//    "fail" in:
//      val order = Order(OrderId("ORDER"), workflow.id /: Position(0), Order.Fresh())
//      def failToPosition(position: Position, uncatchable: Boolean = false) =
//        OrderEventSource.fail(order.id, outcome = None, uncatchable = uncatchable)
//          .calculateEvents(newControllerState(order.withPosition(position)), now).map(_.toList)
//
//      assert(failToPosition(Position(0)) == Right(OrderFailed(Position(0)) :: Nil))
//      assert(failToPosition(Position(0) / BranchId.try_(0) % 0) ==
//        Right(OrderCaught(Position(0) / BranchId.catch_(0) % 0) :: Nil))
//
//      var pos = Position(0) / BranchId.try_(0) % 0
//      assert(failToPosition(pos / BranchId.try_(0) % 0) ==
//        Right(OrderCaught(pos / BranchId.catch_(0) % 0) :: Nil))
//
//      assert(failToPosition(pos / BranchId.catch_(0) % 0) ==
//        Right(OrderCaught(Position(0) / BranchId.catch_(0) % 0) :: Nil))
//
//      pos = Position(0) / BranchId.catch_(0) % 1
//      assert(failToPosition(pos / BranchId.try_(0) % 0) ==
//        Right(OrderCaught(pos / BranchId.catch_(0) % 0) :: Nil))
//
//      assert(failToPosition(pos / BranchId.try_(1) % 0) ==
//        Right(OrderCaught(pos / BranchId.catch_(1) % 0) :: Nil))
//
//      assert(failToPosition(pos / BranchId.try_(2) % 0) ==
//        Right(OrderFailed(pos / BranchId.try_(2) % 0) :: Nil))
//
//      assert(failToPosition(Position(1)) ==
//        Right(OrderFailed(Position(1)) :: Nil))
//
//    "Fresh at try instruction -> OrderMoved" in:
//      val order = Order(OrderId("ORDER"), workflow.id /: Position(0), Order.Fresh())
//      assert(OrderEventSource.nextEvents(order.id)
//        .calculateEvents(newControllerState(order), now).map(_.toList) == Seq(order.id <-:
//        OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 0)))
//
//    "Ready at instruction -> OrderMoved" in:
//      val order = Order(OrderId("ORDER"), workflow.id /: Position(0), Order.Ready())
//      assert(OrderEventSource.nextEvents(order.id)
//        .calculateEvents(newControllerState(order), now).map(_.toList) == Seq(order.id <-:
//        OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 0)))
//
//    "Processed failed in inner try-block -> OrderCaught" in:
//      val pos = Position(0) / try_(0) % 0 / try_(0) % 0
//      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
//        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
//      assert(OrderEventSource.nextEvents(order.id)
//        .calculateEvents(newControllerState(order), now).map(_.toList) == Seq(order.id <-:
//        OrderCaught(Position(0) / try_(0) % 0 / catch_(0) % 0)))
//
//    "Processed failed in inner catch-block -> OrderCaught" in:
//      val pos = Position(0) / try_(0) % 0 / catch_(0) % 0
//      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
//        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
//      assert(OrderEventSource.nextEvents(order.id)
//        .calculateEvents(newControllerState(order), now).map(_.toList) == Seq(order.id <-:
//        OrderCaught(Position(0) / catch_(0) % 0)))
//
//    "Processed failed in outer catch-block -> OrderFailed" in:
//      val pos = Position(0) / catch_(0) % 0
//      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
//        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
//      assert(OrderEventSource.nextEvents(order.id)
//        .calculateEvents(newControllerState(order), now).map(_.toList) == Seq(order.id <-: OrderFailed(pos)))
//
//    "Processed failed in try in catch -> OrderCaught" in:
//      val pos = Position(0) / catch_(0) % 1 / try_(0) % 0
//      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
//        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
//      assert(OrderEventSource.nextEvents(order.id)
//        .calculateEvents(newControllerState(order), now).map(_.toList) == Seq(order.id <-:
//        OrderCaught(Position(0) / catch_(0) % 1 / catch_(0) % 0)))
//
//    "Processed failed in catch in catch -> OrderFailed" in:
//      val pos = Position(0) / catch_(0) % 0
//      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
//        historicOutcomes = Vector(HistoricOutcome(Position(0), failed7)))
//      assert(OrderEventSource.nextEvents(order.id)
//        .calculateEvents(newControllerState(order), now).map(_.toList) == Seq(order.id <-: OrderFailed(pos)))
//
//    "Processed failed not in try/catch -> OrderFailed" in:
//      val pos = Position(1)
//      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
//        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
//      assert(OrderEventSource.nextEvents(order.id)
//        .calculateEvents(newControllerState(order), now).map(_.toList) == Seq(order.id <-: OrderFailed(pos)))
//
//    "Try catch and fork" in:
//      val workflow = WorkflowParser.parse(
//         """define workflow {
//           |  try
//           |    fork(joinIfFailed = true) {
//           |      "🥕": {
//           |        execute agent="a", executable="ex";   // 0/try:0/fork+🥕:0    // FAILS
//           |      },
//           |      "🍋": {
//           |        execute agent="a", executable="ex";   // 0/try:0/fork+🍋:0    // succeeds
//           |      }
//           |    }
//           |  catch {
//           |    execute agent="a", executable="ex";       // 0/catch:0
//           |  };
//           |  execute agent="a", executable="ex";         // 1
//           |}""".stripMargin).orThrow
//      var aChild: Order[Order.State] =
//        val pos = Position(0) / BranchId.try_(0) % 0 / BranchId.fork("🥕") % 0   // Execute
//        Order(OrderId("ORDER|🥕"), workflow.id /: pos, Order.Processed,
//          historicOutcomes = Vector(HistoricOutcome(pos, failed7)),
//          parent = Some(OrderId("ORDER")))
//      var bChild: Order[Order.State] =
//        val pos = Position(0) / BranchId.try_(0) % 0 / BranchId.fork("🍋") % 1   // End
//        Order(OrderId("ORDER|🍋"), workflow.id /: pos, Order.Ready(),
//          historicOutcomes = Vector(HistoricOutcome(pos, failed7)),
//          parent = Some(OrderId("ORDER")))
//      val forkingOrder = Order(OrderId("ORDER"),
//        workflow.id /: (Position(0) / BranchId.try_(0) % 0),  // Fork
//        Order.Forked(Vector(
//          Order.Forked.Child("🥕", aChild.id),
//          Order.Forked.Child("🍋", bChild.id))))
//
//      val controllerState = ControllerTestState.of(
//        orders = Some(Seq(forkingOrder, aChild, bChild)),
//        workflows = Some(Seq(workflow)))
//
//      val orderFailedInFork = OrderFailedInFork(
//        Position(0) / BranchId.try_(0) % 0 / BranchId.fork("🥕") % 0)
//      assert(OrderEventSource.nextEvents(aChild.id).calculateEvents(controllerState, now).map(_.toList) == Right(Seq(aChild.id <-: orderFailedInFork)))
//      aChild = aChild.applyEvent(orderFailedInFork).orThrow
//      bChild = bChild.applyEvent(orderFailedInFork).orThrow
//      // TODO Is FailedInFork replaceable by Ready and !lastOutcome.isSucceeded?
//
//      val orderJoined = OrderJoined(OrderOutcome.Failed(Some(
//        """Order:ORDER|🥕 Failed;
//          |Order:ORDER|🍋 Failed"""
//          .stripMargin)))
//      assert(OrderEventSource.nextEvents(aChild.id      ).calculateEvents(controllerState, now).map(_.toList) == Right(Seq(forkingOrder.id <-: orderJoined)))
//      assert(OrderEventSource.nextEvents(bChild.id      ).calculateEvents(controllerState, now).map(_.toList) == Right(Seq(forkingOrder.id <-: orderJoined)))
//      assert(OrderEventSource.nextEvents(forkingOrder.id).calculateEvents(controllerState, now).map(_.toList) == Right(Seq(forkingOrder.id <-: orderJoined)))
//
//    "Try catch, fork and lock" in:
//      val workflow = WorkflowParser.parse(
//         """define workflow {
//           |  lock (lock="LOCK") {
//           |    try
//           |      fork (joinIfFailed=true) {
//           |        "🥕": {
//           |          execute agent="a", executable="ex";   // 0/lock:0/try:0/fork+🥕:0
//           |        },
//           |        "🍋": {
//           |          lock (lock="LOCK-1") {
//           |            lock (lock="LOCK-2") {
//           |              execute agent="a", executable="ex";   // 0/lock:0/try:0/fork+🍋:0/lock:0/lock:0
//           |            }
//           |          }
//           |        }
//           |      }
//           |    catch {
//           |      execute agent="a", executable="ex";     // 0/lock:&catch:0
//           |    };
//           |  }
//           |}""".stripMargin).orThrow
//      var aChild: Order[Order.State] =
//        val pos = Position(0) / BranchId.Lock % 0 / BranchId.try_(0) % 0 / BranchId.fork("🥕") % 0   // Execute
//        Order(OrderId("ORDER|🥕"), workflow.id /: pos, Order.Processed,
//          historicOutcomes = Vector(HistoricOutcome(pos, failed7)),
//          parent = Some(OrderId("ORDER")))
//      val bChild =
//        val pos = Position(0) / BranchId.Lock % 0 / BranchId.try_(0) % 0 / BranchId.fork("🍋") % 0 / BranchId.Lock % 0 / BranchId.Lock % 0
//        Order(OrderId("ORDER|🍋"), workflow.id /: pos, Order.Processed,
//          historicOutcomes = Vector(HistoricOutcome(pos, failed7)),
//          parent = Some(OrderId("ORDER")))
//      val forkingOrder = Order(OrderId("ORDER"), workflow.id /: (Position(0) / BranchId.Lock % 0 / BranchId.try_(0) % 0),  // Fork
//        Order.Forked(Vector(
//          Order.Forked.Child("🥕", aChild.id),
//          Order.Forked.Child("🍋", bChild.id))))
//      val controllerState = ControllerTestState.of(
//        orders = Some(Seq(forkingOrder, aChild, bChild)),
//        workflows = Some(Seq(workflow)),
//        itemStates = Seq(
//          LockState(Lock(LockPath("LOCK"))),
//          LockState(Lock(LockPath("LOCK-1"))),
//          LockState(Lock(LockPath("LOCK-2")))))
//
//      val orderFailedInFork = OrderFailedInFork(Position(0) / BranchId.Lock % 0 / BranchId.try_(0) % 0 / BranchId.fork("🥕") % 0)
//      assert(OrderEventSource.nextEvents(aChild.id).calculateEvents(controllerState, now).map(_.toList) == Right(Seq(aChild.id <-: orderFailedInFork)))
//      aChild = aChild.applyEvent(orderFailedInFork).orThrow
//
//      assert(OrderEventSource.nextEvents(bChild.id).calculateEvents(controllerState, now).map(_.toList) == Right(Seq(
//        bChild.id <-: OrderLocksReleased(List(LockPath("LOCK-2"))),
//        bChild.id <-: OrderLocksReleased(List(LockPath("LOCK-1"))),
//        bChild.id <-: OrderFailedInFork(
//          Position(0) / BranchId.Lock % 0 / BranchId.try_(0) % 0 / BranchId.fork("🍋") % 0))))
//  }
//
//
//object OrderEventSourceTest:
//
//  private val now = ts"2026-01-21T12:00:00Z"
//  private val TestWorkflowId = WorkflowPath("WORKFLOW") ~ "VERSION"
//  private val ForkWorkflow = ForkTestSetting.TestWorkflow.withId(TestWorkflowId)
//  private val TestAgentPath = AgentPath("AGENT")
//  private val subagentId = SubagentId("SUBAGENT")
//  private val succeededOrderId = OrderId("SUCCESS")
//  private val succeededOrder = Order(succeededOrderId, TestWorkflowId /: Position(0), Order.Processed,
//    historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Succeeded(NamedValues.rc(0)))))
//  private val failedOrder = Order(OrderId("FAILED"), TestWorkflowId /: Position(0), Order.Processed,
//    historicOutcomes = Vector(HistoricOutcome(Position(0), OrderOutcome.Failed(NamedValues.rc(1)))))
//  private val orderForked = OrderForked(Vector(
//    OrderForked.Child("🥕", OrderId("ORDER|🥕")),
//    OrderForked.Child("🍋", OrderId("ORDER|🍋"))))
//
//  private val executeScript = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("executable")))
//
//  private def step(workflow: Workflow, outcome: OrderOutcome): Seq[OrderEvent | PlanFinishedEvent] =
//    val process = new SingleOrderProcess(workflow)
//    process.update(OrderAdded(workflow.id))
//    process.transferToAgent(TestAgentPath)
//    process.update(OrderStarted)
//    process.jobStep(outcome = outcome)
//    process.step()
//
//  final class SingleOrderProcess(val workflow: Workflow, val orderId: OrderId = OrderId("ORDER")):
//    private val process = new Process(workflow)
//
//    def transferToAgent(agentPath: AgentPath) =
//      update(OrderAttachable(agentPath))
//      update(OrderAttached(agentPath))
//
//    def transferToController() =
//      update(OrderDetachable)
//      update(OrderDetached)
//
//    def jobStep(outcome: OrderOutcome = OrderOutcome.Succeeded(NamedValues.rc(0))) =
//      process.jobStep(orderId, outcome)
//
//    def step(): Seq[OrderEvent | NoticeDeleted | PlanFinished  | PlanDeleted] =
//      process.step(orderId).map(_.event)
//
//    def update(event: OrderEvent) =
//      process.update(orderId <-: event)
//
//  final class Process(workflow: Workflow):
//    val idToWorkflow = Map(workflow.id -> workflow)
//    val idToOrder = mutable.Map[OrderId, Order[Order.State]]()
//    private val eventHandler = new OrderEventHandler(idToWorkflow.checked)
//    private val inProcess = mutable.Set.empty[OrderId]
//
//    private def eventSource(isAgent: Boolean) =
//      new OrderEventSource(TestEngineState.of(
//        isAgent = isAgent,
//        orders = Some(idToOrder.values),
//        workflows = Some(idToWorkflow.values)))
//
//    def jobStep(orderId: OrderId, outcome: OrderOutcome = OrderOutcome.succeeded): Unit =
//      update(orderId <-: OrderProcessingStarted(subagentId))
//      update(orderId <-: OrderProcessed(outcome))
//
//    def run(orderId: OrderId): List[KeyedEvent[OrderEvent | PlanFinishedEvent]] =
//      step(orderId) match
//        case keyedEvents if keyedEvents.nonEmpty =>
//          keyedEvents.toList ::: (if idToOrder contains orderId then run(orderId) else Nil)
//        case _ => Nil
//
//    def step(orderId: OrderId): Seq[KeyedEvent[OrderEvent | PlanFinishedEvent]] =
//      val keyedEvents = nextEvents(orderId)
//      keyedEvents foreach update
//      keyedEvents
//
//    private def nextEvents(orderId: OrderId)
//    : Seq[KeyedEvent[OrderEvent | PlanFinishedEvent]] =
//      val order = idToOrder(orderId)
//      if order.detaching.isRight then
//        Seq(order.id <-: OrderDetached)
//      else
//        (order.state, workflow.instruction(order.position)) match
//          case (_: Order.Ready, _: Execute) =>
//            if order.isDetached then
//              Seq(order.id <-: OrderAttachable(TestAgentPath))
//            else if order.isAttaching then
//              Seq(order.id <-: OrderAttached(TestAgentPath))
//            else
//              Seq(order.id <-: OrderProcessingStarted(subagentId))
//
//          case _ if inProcess contains orderId =>
//            Seq(orderId <-: OrderProcessed(OrderOutcome.succeededRC0))
//
//          case _ =>
//            eventSource(isAgent = order.isAttached).nextEvents(orderId)
//
//    def update(keyedEvent: KeyedEvent[OrderEvent | PlanFinishedEvent]): Unit =
//      keyedEvent match
//        case KeyedEvent(orderId: OrderId, event: OrderAdded) =>
//          idToOrder.insert(orderId, Order.fromOrderAdded(orderId, event))
//
//        case KeyedEvent(orderId: OrderId, event: OrderCoreEvent) =>
//          processEvent(keyedEvent)
//          if !event.isInstanceOf[OrderFinished] then
//            idToOrder(orderId) = idToOrder(orderId).applyEvent(event).orThrow
//
//        case _ =>
//          sys.error(s"Unhandled: $keyedEvent")
//
//    private def processEvent(keyedEvent: KeyedEvent[OrderEvent | PlanFinishedEvent])
//    : Unit =
//      keyedEvent match
//        case KeyedEvent(orderId: OrderId, OrderProcessingStarted(_, _, _, _)) =>
//          inProcess += orderId
//
//        case KeyedEvent(orderId: OrderId, _: OrderProcessed) =>
//          inProcess -= orderId
//
//        case KeyedEvent(orderId: OrderId, event: OrderEvent) =>
//          eventHandler
//            .handleEvent(idToOrder(orderId), event)
//            .orThrow
//            .foreach:
//              case FollowUp.AddChild(derivedOrder) =>
//                idToOrder.insert(derivedOrder.id, derivedOrder)
//
//              case FollowUp.Delete(deleteOrderId) =>
//                idToOrder -= deleteOrderId
//
//              case o => sys.error(s"$keyedEvent ~~> $o")
//
//  private def newEngineState(
//    workflow: Workflow,
//    orders: Iterable[Order[Order.State]],
//    isAgent: Boolean) =
//    TestEngineState.of(
//      isAgent = isAgent,
//      orders = Some(orders),
//      workflows = Some(Seq(workflow)))
