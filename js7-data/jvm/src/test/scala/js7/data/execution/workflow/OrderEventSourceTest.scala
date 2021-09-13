package js7.data.execution.workflow

import cats.syntax.option._
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Collections.implicits._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.Problems.CancelStartedOrderProblem
import js7.data.agent.AgentPath
import js7.data.command.CancellationMode.FreshOrStarted
import js7.data.command.{CancellationMode, SuspensionMode}
import js7.data.event.{<-:, KeyedEvent}
import js7.data.execution.workflow.OrderEventSourceTest._
import js7.data.execution.workflow.instructions.InstructionExecutorService.implicits.defaultInstructionExecutorService
import js7.data.job.{PathExecutable, ShellScriptExecutable}
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.OrderEvent.OrderResumed.ReplaceHistoricOutcome
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancellationMarked, OrderCancelled, OrderCatched, OrderCoreEvent, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderLockReleased, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderResumed, OrderResumptionMarked, OrderStarted, OrderSuspended, OrderSuspensionMarked}
import js7.data.order.{HistoricOutcome, Order, OrderEvent, OrderId, OrderMark, Outcome}
import js7.data.problems.{CannotResumeOrderProblem, CannotSuspendOrderProblem, UnreachableOrderPositionProblem}
import js7.data.state.OrderEventHandler.FollowUp
import js7.data.state.{OrderEventHandler, StateView}
import js7.data.value.NamedValues
import js7.data.value.expression.Expression.{BooleanConstant, Equal, LastReturnCode, NumericConstant}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ExplicitEnd, Fail, Fork, Gap, Goto, If, IfFailedGoto, LockInstruction, TryInstruction}
import js7.data.workflow.position.BranchId.{Else, Then, catch_, try_}
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.test.ForkTestSetting
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class OrderEventSourceTest extends AnyFreeSpec
{
  "JobSchedulerRestarted" in {
    val eventSource = new OrderEventSource(
      StateView.forTest(
        isAgent = false,
        idToOrder = Map(
          disruptedOrder.id -> disruptedOrder),
        idToWorkflow = Map(
          TestWorkflowId -> ForkWorkflow)))
    assert(eventSource.nextEvents(disruptedOrder.id) ==
      List(disruptedOrder.id <-: OrderMoved(disruptedOrder.position)))  // Move to same InstructionNr
  }

  "if" - {
    val workflow = Workflow.of(TestWorkflowId,
      executeScript,                                  // 0
      If(Equal(LastReturnCode, NumericConstant(0)),   // 1
        Workflow.of(executeScript)),                  // 1/then:0
      executeScript)                                  // 2

    "then branch executed" in {
      assert(step(workflow, Outcome.Succeeded(NamedValues.rc(0))) == Seq(OrderMoved(Position(1) / Then % 0)))
    }

    "then branch skipped" in {
      assert(step(workflow, Outcome.Succeeded(NamedValues.rc(1))) == Seq(OrderMoved(Position(2))))
    }

    "again, all events" in {
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
      assert(process.step() == Seq(OrderFinished))
    }

    "then branch not executed" in {
      assert(step(workflow, Outcome.Succeeded(NamedValues.rc(1))) == Seq(OrderMoved(Position(2))))
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
      assert(step(workflow, Outcome.succeededRC0) == Seq(OrderMoved(Position(1) / Then % 0)))
    }

    "else branch executed" in {
      assert(step(workflow, Outcome.Succeeded(NamedValues.rc(1))) == Seq(OrderMoved(Position(1) / Else % 0)))
    }
  }

  "fork" in {
    val process = new Process(ForkWorkflow)
    val orderId = succeededOrderId

    process.update(orderId <-: OrderAdded(TestWorkflowId))
    process.update(orderId <-: OrderAttachable(TestAgentPath))
    process.update(orderId <-: OrderAttached(TestAgentPath))
    assert(process.run(orderId) == List(
      orderId <-: OrderStarted,
      orderId <-: OrderDetachable,
      orderId <-: OrderDetached,
      orderId <-: OrderForked(Vector(
        OrderForked.Child("🥕", orderId | "🥕"),
        OrderForked.Child("🍋", orderId | "🍋")))))

    assert(process.run(orderId | "🥕") == List(
      (orderId | "🥕") <-: OrderAttachable(TestAgentPath),
      (orderId | "🥕") <-: OrderAttached(TestAgentPath),
      (orderId | "🥕") <-: OrderProcessingStarted,
      (orderId | "🥕") <-: OrderProcessed(Outcome.succeededRC0),
      (orderId | "🥕") <-: OrderMoved(Position(0) / "fork+🥕" % 1),
      (orderId | "🥕") <-: OrderDetachable,
      (orderId | "🥕") <-: OrderDetached))

    assert(process.step(orderId).isEmpty)  // Nothing to join

    assert(process.run(orderId | "🍋") == List(
      (orderId | "🍋") <-: OrderAttachable(TestAgentPath),
      (orderId | "🍋") <-: OrderAttached(TestAgentPath),
      (orderId | "🍋") <-: OrderProcessingStarted,
      (orderId | "🍋") <-: OrderProcessed(Outcome.succeededRC0),
      (orderId | "🍋") <-: OrderMoved(Position(0) / "fork+🍋" % 1),
      (orderId | "🍋") <-: OrderDetachable,
      (orderId | "🍋") <-: OrderDetached,
      orderId <-: OrderJoined(Outcome.succeeded)))
    assert(process.step(orderId) == Seq(orderId <-: OrderMoved(Position(1))))

    assert(process.step(orderId) == Seq(orderId <-: OrderForked(Vector(
      OrderForked.Child("🥕", orderId | "🥕"),
      OrderForked.Child("🍋", orderId | "🍋")))))

    assert(process.run(orderId | "🥕") == List(
      (orderId | "🥕") <-: OrderAttachable(TestAgentPath),
      (orderId | "🥕") <-: OrderAttached(TestAgentPath),
      (orderId | "🥕") <-: OrderProcessingStarted,
      (orderId | "🥕") <-: OrderProcessed(Outcome.succeededRC0),
      (orderId | "🥕") <-: OrderMoved(Position(1) / "fork+🥕" % 1),
      (orderId | "🥕") <-: OrderDetachable,
      (orderId | "🥕") <-: OrderDetached))

    assert(process.step(orderId).isEmpty)  // Nothing to join

    assert(process.run(orderId | "🍋") == List(
      (orderId | "🍋") <-: OrderAttachable(TestAgentPath),
      (orderId | "🍋") <-: OrderAttached(TestAgentPath),
      (orderId | "🍋") <-: OrderProcessingStarted,
      (orderId | "🍋") <-: OrderProcessed(Outcome.succeededRC0),
      (orderId | "🍋") <-: OrderMoved(Position(1) / "fork+🍋" % 1),
      (orderId | "🍋") <-: OrderDetachable,
      (orderId | "🍋") <-: OrderDetached,
      orderId <-: OrderJoined(Outcome.succeeded)))

    assert(process.step(orderId) == Seq(orderId <-: OrderMoved(Position(2))))
    assert(process.step(orderId) == Seq(orderId <-: OrderAttachable(TestAgentPath)))
    assert(process.step(orderId) == Seq(orderId <-: OrderAttached(TestAgentPath)))
    assert(process.step(orderId) == Seq(orderId <-: OrderProcessingStarted))
    // and so forth...
  }

  "applyMoveInstructions" - {
    for (isAgent <- Seq(false, true)) s"isAgent=$isAgent" - {
      "Goto, IfFailedGoto" in {
        val workflow = Workflow.of(TestWorkflowId,
                   executeScript,  // 0
                   Goto("B"),      // 1
                   Gap.empty,      // 2
          "C" @:   executeScript,  // 3
          "END" @: ExplicitEnd(),  // 4
          "B" @:   IfFailedGoto("C"), // 5
                   TryInstruction(               // 6
                     Workflow.of(executeScript),  // 6/0:0
                     Workflow.of(executeScript))) // 6/1:0
        val eventSource = newWorkflowEventSource(workflow, List(succeededOrder, failedOrder), isAgent = isAgent)
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
        val eventSource = newWorkflowEventSource(workflow, List(succeededOrder, failedOrder), isAgent = isAgent)
        assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(0)) == Left(Problem("Order:SUCCESS is in a workflow loop: 1 B: goto A --> 0 A: goto B")))
        assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1)) == Left(Problem("Order:SUCCESS is in a workflow loop: 0 A: goto B --> 1 B: goto A")))
        assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(2)) == Right(Position(3)))
        assert(eventSource.applyMoveInstructions(failedOrder    withPosition Position(2)) == Left(Problem("Order:FAILED is in a workflow loop: 0 A: goto B --> 1 B: goto A")))
      }

      "Job, Fork" in {
        val eventSource = newWorkflowEventSource(ForkWorkflow, List(succeededOrder, failedOrder), isAgent = isAgent)
        assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(0)) == Right(Position(0)))
        assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1)) == Right(Position(1)))
      }

      "In forked order" in {
        val eventSource = newWorkflowEventSource(ForkWorkflow, List(succeededOrder, failedOrder), isAgent = isAgent)
        assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1) / "fork+🥕" % 1) == Right(Position(1) / "fork+🥕" % 1))
      }
    }
  }

  "cancel, suspend, resume" - {
    val detached = none[Order.AttachedState]
    val attaching = Some(Order.Attaching(TestAgentPath))
    val attached = Some(Order.Attached(TestAgentPath))
    val detaching = Some(Order.Detaching(TestAgentPath))

    val historicOps = Seq(OrderResumed.ReplaceHistoricOutcome(Position(0), Outcome.failed))

    "Order.mark.isEmpty" - {
      val unmarkedOrder = Order(OrderId("ORDER"), TestWorkflowId, Order.Fresh)

      "Fresh" - {
        val freshOrder = unmarkedOrder

        "Detached" in {
          testEventSource(freshOrder, detached) { (order, controller, _) =>
            assert(controller.nextEvents(order.id) == Seq(order.id <-: OrderStarted))
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Some(Seq(OrderCancelled))))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancelled))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderSuspended))))
            assert(controller.resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
          }
        }

        "Attaching" in {
          testEventSource(freshOrder, attaching) { (order, controller, agent) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(agent     .nextEvents(order.id) == Nil)
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderSuspensionMarked()))))
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderSuspensionMarked()))))
            assert(controller.resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
          }
        }

        "Attached" in {
          testEventSource(freshOrder, attached) { (order, controller, agent) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(agent     .nextEvents(order.id) == Seq(order.id <-: OrderStarted))
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Right(Some(Seq(OrderDetachable))))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderDetachable))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderSuspensionMarked()))))
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderDetachable))))
            assert(controller.resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
          }
        }

        "Detaching" in {
          testEventSource(freshOrder, detaching) { (order, controller, agent) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(agent     .nextEvents(order.id) == Nil)
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderSuspensionMarked()))))
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderSuspensionMarked()))))
            assert(controller.resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
          }
        }
      }

      "Ready" - {
        val readyOrder = unmarkedOrder.copy(state = Order.Ready)

        "Detached" in {
          testEventSource(readyOrder, attachedState = detached) { (order, controller, _) =>
            assert(controller.nextEvents(order.id) == Seq(order.id <-: orderForked))
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancelled))))
          }
        }

        "Attaching" in {
          testEventSource(readyOrder, attaching) { (order, controller, agent) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(agent     .nextEvents(order.id) == Nil)
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderSuspensionMarked()))))
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderSuspensionMarked()))))
            assert(controller.resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
          }
        }

        "Attached" in {
          testEventSource(readyOrder, attached) { (order, controller, agent) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(agent     .nextEvents(order.id) == Seq(order.id <-: OrderDetachable))
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderDetachable))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderSuspensionMarked()))))
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderDetachable))))
            assert(controller.resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
          }
        }

        "Detaching" in {
          testEventSource(readyOrder, detaching) { (order, controller, agent) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(agent     .nextEvents(order.id) == Nil)
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderSuspensionMarked()))))
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderSuspensionMarked()))))
            assert(controller.resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
          }
        }
      }

      "Processing Attached" in {
        val processingOrder = unmarkedOrder.copy(state = Order.Processing)
        testEventSource(processingOrder, attached) { (order, controller, agent) =>
          assert(controller.nextEvents(order.id) == Nil)
          assert(agent     .nextEvents(order.id) == Nil)
          assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
          assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
          assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
          assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
          assert(controller.suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderSuspensionMarked()))))
          assert(agent     .suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderSuspensionMarked()))))
          assert(controller.resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
          assert(agent     .resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
        }
      }
    }

    "OrderMark.Cancelling(FreshOnly)" - {
      val cancellingOrder = Order(OrderId("ORDER"), TestWorkflowId, Order.Fresh,
        mark = Some(OrderMark.Cancelling(CancellationMode.FreshOnly)))

      "Fresh" - {
        val freshOrder = cancellingOrder

        "Detached" in {
          testEventSource(freshOrder, detached) { (order, controller, _) =>
            assert(controller.nextEvents(order.id) == Seq(order.id <-: OrderCancelled))
          }
        }

        "Attached" in {
          testEventSource(freshOrder, attached) { (order, controller, agent) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(agent     .nextEvents(order.id) == Seq(order.id <-: OrderDetachable))
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(None))
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Right(Some(Seq(OrderDetachable))))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(FreshOrStarted(None))))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderDetachable))))
            assert(controller.suspend(order.id, SuspensionMode()) == Left(CannotSuspendOrderProblem))
            assert(agent     .suspend(order.id, SuspensionMode()) == Left(CannotSuspendOrderProblem))
            assert(controller.resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
            assert(controller.resume(order.id, Some(Position(1)), Nil) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, Some(Position(1)), Nil) == Left(CannotResumeOrderProblem))
          }
        }
      }
    }

    "OrderMark.Cancelling(FreshOrStarted)" - {
      val cancellingOrder = Order(OrderId("ORDER"), TestWorkflowId, Order.Fresh, mark = Some(OrderMark.Cancelling(CancellationMode.FreshOnly)))

      "Ready" - {
        val readyOrder = cancellingOrder.copy(state = Order.Ready)

        "Detached" in {
          testEventSource(readyOrder, detached) { (order, controller, _) =>
            assert(controller.nextEvents(order.id) == Seq(order.id <-: orderForked))
          }
        }

        "Attached" in {
          testEventSource(readyOrder, attached) { (order, controller, agent) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(agent     .nextEvents(order.id) == Seq(order.id <-: OrderDetachable))
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(FreshOrStarted(None))))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderDetachable))))
            assert(controller.suspend(order.id, SuspensionMode()) == Left(CannotSuspendOrderProblem))
            assert(agent     .suspend(order.id, SuspensionMode()) == Left(CannotSuspendOrderProblem))
            assert(controller.resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
            assert(controller.resume(order.id, Some(Position(1)), Nil) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, Some(Position(1)), Nil) == Left(CannotResumeOrderProblem))
          }
        }
      }

      "Processing Attached" in {
        val processingOrder = cancellingOrder.copy(state = Order.Processing)
        testEventSource(processingOrder, attached) { (order, controller, agent) =>
          assert(controller.nextEvents(order.id) == Nil)
          assert(agent     .nextEvents(order.id) == Nil)
          assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
          assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
          assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(FreshOrStarted(None))))))
          assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(FreshOrStarted(None))))))
          assert(controller.suspend(order.id, SuspensionMode()) == Left(CannotSuspendOrderProblem))
          assert(agent     .suspend(order.id, SuspensionMode()) == Left(CannotSuspendOrderProblem))
          assert(controller.resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
          assert(agent     .resume(order.id, None, Nil) == Left(CannotResumeOrderProblem))
          assert(controller.resume(order.id, Some(Position(1)), Nil) == Left(CannotResumeOrderProblem))
          assert(agent     .resume(order.id, Some(Position(1)), Nil) == Left(CannotResumeOrderProblem))
        }
      }
    }

    "OrderMark.Suspending" - {
      val suspendingOrder = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready, mark = Some(OrderMark.Suspending()))

      "Ready" - {
        val readyOrder = suspendingOrder

        "Detached" in {
          testEventSource(readyOrder, detached) { (order, controller, _) =>
            assert(controller.nextEvents(order.id) == Seq(order.id <-: OrderSuspended))
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancelled))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(None))
            assert(controller.resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
            assert(controller.resume(order.id, Some(Position(1)), Nil) == Left(CannotResumeOrderProblem))
            assert(controller.resume(order.id, Some(Position(1)), Seq(ReplaceHistoricOutcome(Position(0), Outcome.succeeded))) == Left(CannotResumeOrderProblem))
            assert(controller.resume(order.id, None, Seq(ReplaceHistoricOutcome(Position(0), Outcome.succeeded))) == Left(CannotResumeOrderProblem))
          }
        }

        "Attached" in {
          testEventSource(readyOrder, attached) { (order, controller, agent) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(agent     .nextEvents(order.id) == Seq(order.id <-: OrderDetachable))
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderDetachable))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(None))
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(None))
            assert(controller.resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
            assert(agent     .resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
            assert(controller.resume(order.id, Some(Position(1)), Nil) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, Some(Position(1)), Nil) == Left(CannotResumeOrderProblem))
            assert(controller.resume(order.id, None, historicOps) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None, historicOps) == Left(CannotResumeOrderProblem))
          }
        }
      }

      "Processing Attached" in {
        val processingOrder = suspendingOrder.copy(state = Order.Processing)
        testEventSource(processingOrder, attached) { (order, controller, agent) =>
          assert(controller.nextEvents(order.id) == Nil)
          assert(agent     .nextEvents(order.id) == Nil)
          assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
          assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
          assert(controller.suspend(order.id, SuspensionMode()) == Right(None))
          assert(agent     .suspend(order.id, SuspensionMode()) == Right(None))
          assert(controller.resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
          assert(agent     .resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
          assert(controller.resume(order.id, Some(Position(1)), Nil) == Left(CannotResumeOrderProblem))
          assert(agent     .resume(order.id, Some(Position(1)), Nil) == Left(CannotResumeOrderProblem))
          assert(controller.resume(order.id, None, historicOps) == Left(CannotResumeOrderProblem))
          assert(agent     .resume(order.id, None, historicOps) == Left(CannotResumeOrderProblem))
        }
      }
    }

    "OrderMark.Resuming isSuspended" - {
      val resumingOrder = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready, mark = Some(OrderMark.Resuming()), isSuspended = true)

      "Ready" - {
        val readyOrder = resumingOrder

        "Detached" in {
          testEventSource(readyOrder, detached) { (order, controller, _) =>
            assert(controller.nextEvents(order.id) == Seq(order.id <-: OrderResumed()))
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancelled))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderSuspended))))
            assert(controller.resume(order.id, None, Nil) == Right(Some(Seq(OrderResumed())/*should already be happened*/)))
            assert(controller.resume(order.id, Some(Position(1)), Nil) == Left(CannotResumeOrderProblem))
          }
        }

        "Attached" in {
          testEventSource(readyOrder, attached) { (order, controller, agent) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(agent     .nextEvents(order.id) == Seq(order.id <-: OrderResumed()))
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderDetachable))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(None))
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderDetachable))))
            assert(controller.resume(order.id, None, Nil) == Right(None))
            assert(agent     .resume(order.id, None, Nil) == Right(None))
            assert(controller.resume(order.id, Some(Position(1)), Nil) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, Some(Position(1)), Nil) == Left(CannotResumeOrderProblem))
          }
        }
      }
    }

    "Order.isSuspended" - {
      val suspendedOrder = Order(OrderId("ORDER"), TestWorkflowId, Order.Fresh, isSuspended = true)

      "Fresh" - {
        val freshOrder = suspendedOrder

        "Detached" in {
          testEventSource(freshOrder, detached) { (order, controller, _) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Some(Seq(OrderCancelled))))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancelled))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(None))
            assert(controller.resume(order.id, None, Nil) == Right(Some(Seq(OrderResumed()))))
            assert(controller.resume(order.id, Some(Position(1)), Nil) == Right(Some(Seq(OrderResumed(Some(Position(1)))))))
            assert(controller.resume(order.id, Some(Position(99)), Nil) == Left(UnreachableOrderPositionProblem))
          }
        }

        "Attaching" in {
          testEventSource(freshOrder, attaching) { (order, controller, agent) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(agent     .nextEvents(order.id) == Nil)
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(None))
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(None))
            assert(controller.resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
            assert(agent     .resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
            assert(controller.resume(order.id, Some(Position(1)), Nil) == Right(Some(Seq(OrderResumptionMarked(Some(Position(1)))))))
            assert(agent     .resume(order.id, Some(Position(1)), Nil) == Right(Some(Seq(OrderResumptionMarked(Some(Position(1)))))))
            assert(controller.resume(order.id, Some(Position(1)), historicOps) == Right(Some(Seq(OrderResumptionMarked(Some(Position(1)), historicOps)))))
            assert(agent     .resume(order.id, Some(Position(1)), historicOps) == Right(Some(Seq(OrderResumptionMarked(Some(Position(1)), historicOps)))))
          }
        }

        "Attached" in {
          testEventSource(freshOrder, attached) { (order, controller, agent) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(agent     .nextEvents(order.id) == Nil)
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Right(Some(Seq(OrderDetachable))))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderDetachable))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(None))
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(Some(Seq(OrderDetachable))))
            assert(controller.resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
            assert(agent     .resume(order.id, None, Nil) == Right(Some(Seq(OrderResumed()))))
            assert(controller.resume(order.id, Some(Position(1)), Nil) == Right(Some(Seq(OrderResumptionMarked(Some(Position(1)))))))
            assert(agent     .resume(order.id, Some(Position(1)), Nil) == Right(Some(Seq(OrderResumed(Some(Position(1)))))))
          }
        }

        "Detaching" in {
          testEventSource(freshOrder, detaching) { (order, controller, agent) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(agent     .nextEvents(order.id) == Nil)
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOnly)))))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(None))
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(None))
            assert(controller.resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
            assert(agent     .resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
            assert(controller.resume(order.id, Some(Position(1)), Nil) == Right(Some(Seq(OrderResumptionMarked(Some(Position(1)))))))
            assert(agent     .resume(order.id, Some(Position(1)), Nil) == Right(Some(Seq(OrderResumptionMarked(Some(Position(1)))))))
          }
        }
      }

      "Ready" - {
        val readyOrder = suspendedOrder.copy(state = Order.Ready)

        "Detached" in {
          testEventSource(readyOrder, detached) { (order, controller, _) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(controller.cancel(order.id, CancellationMode.FreshOnly     ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancelled))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(None))
            assert(controller.resume(order.id, None, Nil) == Right(Some(Seq(OrderResumed()))))
            assert(controller.resume(order.id, Some(Position(1)), Nil) == Right(Some(Seq(OrderResumed(Some(Position(1)))))))
          }
        }

        "Attaching" in {
          testEventSource(readyOrder, attaching) { (order, controller, agent) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(agent     .nextEvents(order.id) == Nil)
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(None))
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(None))
            assert(controller.resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
            assert(agent     .resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
            assert(controller.resume(order.id, Some(Position(1)), Nil) == Right(Some(Seq(OrderResumptionMarked(Some(Position(1)))))))
            assert(agent     .resume(order.id, Some(Position(1)), Nil) == Right(Some(Seq(OrderResumptionMarked(Some(Position(1)))))))
          }
        }

        "Attached" in {
          testEventSource(readyOrder, attached) { (order, controller, agent) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(agent     .nextEvents(order.id) == Nil)
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderDetachable))))
            assert(controller.resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
            assert(agent     .resume(order.id, None, Nil) == Right(Some(Seq(OrderResumed()))))
            assert(controller.resume(order.id, Some(Position(1)), Nil) == Right(Some(Seq(OrderResumptionMarked(Some(Position(1)))))))
            assert(agent     .resume(order.id, Some(Position(1)), Nil) == Right(Some(Seq(OrderResumed(Some(Position(1)))))))
          }
        }

        "Detaching" in {
          testEventSource(readyOrder, detaching) { (order, controller, agent) =>
            assert(controller.nextEvents(order.id) == Nil)
            assert(agent     .nextEvents(order.id) == Nil)
            assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
            assert(controller.suspend(order.id, SuspensionMode()) == Right(None))
            assert(agent     .suspend(order.id, SuspensionMode()) == Right(None))
            assert(controller.resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
            assert(agent     .resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
            assert(controller.resume(order.id, Some(Position(1)), Nil) == Right(Some(Seq(OrderResumptionMarked(Some(Position(1)))))))
            assert(agent     .resume(order.id, Some(Position(1)), Nil) == Right(Some(Seq(OrderResumptionMarked(Some(Position(1)))))))
          }
        }
      }

      "Processing Attached" in {
        val processingOrder = suspendedOrder.copy(state = Order.Processing)
        testEventSource(processingOrder, attached) { (order, controller, agent) =>
          assert(controller.nextEvents(order.id) == Nil)
          assert(agent     .nextEvents(order.id) == Nil)
          assert(controller.cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
          assert(agent     .cancel(order.id, CancellationMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
          assert(controller.cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
          assert(agent     .cancel(order.id, CancellationMode.FreshOrStarted()) == Right(Some(Seq(OrderCancellationMarked(CancellationMode.FreshOrStarted())))))
          assert(controller.suspend(order.id, SuspensionMode()) == Right(None))
          assert(agent     .suspend(order.id, SuspensionMode()) == Right(None))
          assert(controller.resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
          assert(agent     .resume(order.id, None, Nil) == Right(Some(Seq(OrderResumptionMarked()))))
          assert(controller.resume(order.id, Some(Position(1)), Nil) == Right(Some(Seq(OrderResumptionMarked(Some(Position(1)))))))
          assert(agent     .resume(order.id, Some(Position(1)), Nil) == Right(Some(Seq(OrderResumptionMarked(Some(Position(1)))))))
        }
      }
    }

    def testEventSource(templateOrder: Order[Order.State], attachedState: Option[Order.AttachedState])
      (body: (Order[Order.State], OrderEventSource, OrderEventSource) => Unit)
    = {
      val order = templateOrder.copy(attachedState = attachedState)
      def eventSource(isAgent: Boolean) = new OrderEventSource(StateView.forTest(
        isAgent = isAgent,
        idToOrder = Map(order.id -> order),
        idToWorkflow = Map(TestWorkflowId -> ForkWorkflow)))
      body(order, eventSource(isAgent = false), eventSource(isAgent = true))
    }

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
        /*4*/ LockInstruction(lockPath, count = None, Workflow.of(execute))))

      "Same level" in {
        assert(testResume(workflow, Position(0), Position(1)) == Right(Some(Seq(OrderResumed(Some(Position(1)))))))
      }

      "Into and out of if-then" in {
        assert(isResumableBackAndForth(workflow, Position(0), Position(1) / BranchId.Then % 0))
      }

      "Into and out of if-else" in {
        assert(isResumableBackAndForth(workflow, Position(0), Position(1) / BranchId.Else % 0))
      }

      "Into and out of try" in {
        assert(isResumableBackAndForth(workflow, Position(0), Position(2) / BranchId.Try_ % 0))
        assert(isResumableBackAndForth(workflow, Position(0), Position(2) / BranchId.try_(3) % 0))
      }

      "Into and out of catch" in {
        assert(isResumableBackAndForth(workflow, Position(0), Position(2) / BranchId.Catch_ % 0))
        assert(isResumableBackAndForth(workflow, Position(0), Position(2) / BranchId.catch_(3) % 0))
      }

      "Into a fork is forbidden" in {
        assert(testResume(workflow, Position(0), Position(3) / BranchId.fork("A") % 0) ==
          Left(UnreachableOrderPositionProblem))
      }

      "Leaving a fork is forbidden" in {
        assert(testResume(workflow, Position(3) / BranchId.fork("A") % 0, Position(0)) ==
          Left(UnreachableOrderPositionProblem))
      }

      "Into a lock is forbidden" in {
        assert(testResume(workflow, Position(0), Position(4) / BranchId.Lock % 0) ==
          Left(UnreachableOrderPositionProblem))
      }

      "Leaving a lock is forbidden" in {
        assert(testResume(workflow, Position(4) / BranchId.Lock % 0, Position(0)) ==
          Left(UnreachableOrderPositionProblem))
      }

      def isResumableBackAndForth(workflow: Workflow, a: Position, b: Position) =
        isResumable(workflow, a, b) && isResumable(workflow, b, a)

      def isResumable(workflow: Workflow, from: Position, to: Position) =
        testResume(workflow, from, to) == Right(Some(OrderResumed(Some(to)) :: Nil))

      def testResume(workflow: Workflow, from: Position, to: Position)
      : Checked[Option[List[OrderEvent.OrderActorEvent]]] = {
        val order = Order(OrderId("SUSPENDED"), workflow.id /: from, Order.Ready, isSuspended = true)
        def eventSource = new OrderEventSource(StateView.forTest(
          isAgent = false,
          idToOrder = Map(order.id -> order),
          idToWorkflow = Map(workflow.id -> workflow),
          pathToLockState = Map(lockPath -> LockState(Lock(lockPath)))))
        eventSource.resume(order.id, Some(to), Nil)
      }
    }
  }

  "Failed" in {
    lazy val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1", Vector(Fail()))
    val order = Order(OrderId("ORDER"), workflow.id /: Position(0), Order.Failed)
    val eventSource = new OrderEventSource(StateView.forTest(
      isAgent = false,
      idToOrder = Map(order.id -> order),
      idToWorkflow = Map(workflow.id -> workflow)))
    assert(eventSource.nextEvents(order.id) == Nil)
    assert(eventSource.suspend(order.id, SuspensionMode()) == Left(CannotSuspendOrderProblem))
    assert(eventSource.cancel(order.id, CancellationMode.Default) ==
      Right(Some(OrderCancelled :: Nil)))
    assert(eventSource.resume(order.id, None, Nil) ==
      Right(Some(OrderResumed(None, Nil) :: Nil)))
  }

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
      new OrderEventSource(StateView.forTest(
        isAgent = false,
        idToOrder = Map(order.id -> order),
        idToWorkflow = Map(workflow.id -> workflow)))

    val failed7 = Outcome.Failed(NamedValues.rc(7))

    "failToPosition" in {
      val order = Order(OrderId("ORDER"), workflow.id, Order.Fresh)
      def failToPosition(position: Position, uncatchable: Boolean = false) =
        eventSource(order).failToPosition(workflow, order.withPosition(position), outcome = None, uncatchable = uncatchable)

      assert(failToPosition(Position(0)) == Right(OrderFailed(Position(0)) :: Nil))
      assert(failToPosition(Position(0) / BranchId.try_(0) % 0) ==
        Right(OrderCatched(Position(0) / BranchId.catch_(0) % 0) :: Nil))

      var pos = Position(0) / BranchId.try_(0) % 0
      assert(failToPosition(pos / BranchId.try_(0) % 0) ==
        Right(OrderCatched(pos / BranchId.catch_(0) % 0) :: Nil))

      assert(failToPosition(pos / BranchId.catch_(0) % 0) ==
        Right(OrderCatched(Position(0) / BranchId.catch_(0) % 0) :: Nil))

      pos = Position(0) / BranchId.catch_(0) % 1
      assert(failToPosition(pos / BranchId.try_(0) % 0) ==
        Right(OrderCatched(pos / BranchId.catch_(0) % 0) :: Nil))

      assert(failToPosition(pos / BranchId.try_(1) % 0) ==
        Right(OrderCatched(pos / BranchId.catch_(1) % 0) :: Nil))

      assert(failToPosition(pos / BranchId.try_(2) % 0) ==
        Right(OrderFailed(pos / BranchId.try_(2) % 0) :: Nil))

      assert(failToPosition(Position(1)) ==
        Right(OrderFailed(Position(1)) :: Nil))
    }

    "Fresh at try instruction -> OrderMoved" in {
      val order = Order(OrderId("ORDER"), workflow.id, Order.Fresh)
      assert(eventSource(order).nextEvents(order.id) == Seq(order.id <-:
        OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 0)))
    }

    "Ready at instruction -> OrderMoved" in {
      val order = Order(OrderId("ORDER"), workflow.id, Order.Ready)
      assert(eventSource(order).nextEvents(order.id) == Seq(order.id <-:
        OrderMoved(Position(0) / try_(0) % 0 / try_(0) % 0)))
    }

    "Processed failed in inner try-block -> OrderCatched" in {
      val pos = Position(0) / try_(0) % 0 / try_(0) % 0
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
      assert(eventSource(order).nextEvents(order.id) == Seq(order.id <-:
        OrderCatched(Position(0) / try_(0) % 0 / catch_(0) % 0)))
    }

    "Processed failed in inner catch-block -> OrderCatched" in {
      val pos = Position(0) / try_(0) % 0 / catch_(0) % 0
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
      assert(eventSource(order).nextEvents(order.id) == Seq(order.id <-:
        OrderCatched(Position(0) / catch_(0) % 0)))
    }

    "Processed failed in outer catch-block -> OrderFailed" in {
      val pos = Position(0) / catch_(0) % 0
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
      assert(eventSource(order).nextEvents(order.id) == Seq(order.id <-: OrderFailed(pos)))
    }

    "Processed failed in try in catch -> OrderCatched" in {
      val pos = Position(0) / catch_(0) % 1 / try_(0) % 0
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
      assert(eventSource(order).nextEvents(order.id) == Seq(order.id <-:
        OrderCatched(Position(0) / catch_(0) % 1 / catch_(0) % 0)))
    }

    "Processed failed in catch in catch -> OrderFailed" in {
      val pos = Position(0) / catch_(0) % 0
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(Position(0), failed7)))
      assert(eventSource(order).nextEvents(order.id) == Seq(order.id <-: OrderFailed(pos)))
    }

    "Processed failed not in try/catch -> OrderFailed" in {
      val pos = Position(1)
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = Vector(HistoricOutcome(pos, failed7)))
      assert(eventSource(order).nextEvents(order.id) == Seq(order.id <-: OrderFailed(pos)))
    }

    "Try catch and fork" in {
      val workflow = WorkflowParser.parse(
         """define workflow {
           |  try
           |    fork {
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
      var aChild: Order[Order.State] = {
        val pos = Position(0) / BranchId.try_(0) % 0 / BranchId.fork("🥕") % 0   // Execute
        Order(OrderId("ORDER|🥕"), workflow.id /: pos, Order.Processed,
          historicOutcomes = Vector(HistoricOutcome(pos, failed7)),
          parent = Some(OrderId("ORDER")))
      }
      val bChild = {
        val pos = Position(0) / BranchId.try_(0) % 0 / BranchId.fork("🍋") % 1   // End
        Order(OrderId("ORDER|🍋"), workflow.id /: pos, Order.Ready,
          historicOutcomes = Vector(HistoricOutcome(pos, failed7)),
          parent = Some(OrderId("ORDER")))
      }
      val forkingOrder = Order(OrderId("ORDER"), workflow.id /: (Position(0) / BranchId.try_(0) % 0),  // Fork
        Order.Forked(Vector(
          Order.Forked.Child("🥕", aChild.id),
          Order.Forked.Child("🍋", bChild.id))))

      def eventSource = new OrderEventSource(StateView.forTest(
        isAgent = false,
        idToOrder = Map(
          forkingOrder.id -> forkingOrder,
          aChild.id -> aChild,
          bChild.id -> bChild),
        idToWorkflow = Map(
          workflow.id -> workflow)))

      val orderFailedInFork = OrderFailedInFork(
        Position(0) / BranchId.try_(0) % 0 / BranchId.fork("🥕") % 0)
      assert(eventSource.nextEvents(aChild.id) == Seq(aChild.id <-: orderFailedInFork))
      aChild = aChild.applyEvent(orderFailedInFork).orThrow

      val orderJoined = OrderJoined(Outcome.Failed(Some(
        "Order:ORDER|🥕 failed;\nOrder:ORDER|🍋 failed")))
      assert(eventSource.nextEvents(aChild.id      ) == Seq(forkingOrder.id <-: orderJoined))
      assert(eventSource.nextEvents(bChild.id      ) == Seq(forkingOrder.id <-: orderJoined))
      assert(eventSource.nextEvents(forkingOrder.id) == Seq(forkingOrder.id <-: orderJoined))
    }

    "Try catch, fork and lock" in {
      val workflow = WorkflowParser.parse(
         """define workflow {
           |  lock (lock="LOCK") {
           |    try
           |      fork {
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
      var aChild: Order[Order.State] = {
        val pos = Position(0) / BranchId.Lock % 0 / BranchId.try_(0) % 0 / BranchId.fork("🥕") % 0   // Execute
        Order(OrderId("ORDER|🥕"), workflow.id /: pos, Order.Processed,
          historicOutcomes = Vector(HistoricOutcome(pos, failed7)),
          parent = Some(OrderId("ORDER")))
      }
      val bChild = {
        val pos = Position(0) / BranchId.Lock % 0 / BranchId.try_(0) % 0 / BranchId.fork("🍋") % 0 / BranchId.Lock % 0 / BranchId.Lock % 0
        Order(OrderId("ORDER|🍋"), workflow.id /: pos, Order.Processed,
          historicOutcomes = Vector(HistoricOutcome(pos, failed7)),
          parent = Some(OrderId("ORDER")))
      }
      val forkingOrder = Order(OrderId("ORDER"), workflow.id /: (Position(0) / BranchId.Lock % 0 / BranchId.try_(0) % 0),  // Fork
        Order.Forked(Vector(
          Order.Forked.Child("🥕", aChild.id),
          Order.Forked.Child("🍋", bChild.id))))
      def liveEventSource = new OrderEventSource(StateView.forTest(
        isAgent = false,
        idToOrder = Map(
          forkingOrder.id -> forkingOrder,
          aChild.id -> aChild,
          bChild.id -> bChild),
        idToWorkflow = Map(
          workflow.id -> workflow),
        pathToLockState = Map(
          LockPath("LOCK") -> LockState(Lock(LockPath("LOCK"))),
          LockPath("LOCK-1") -> LockState(Lock(LockPath("LOCK-1"))),
          LockPath("LOCK-2") -> LockState(Lock(LockPath("LOCK-2"))))))

      val orderFailedInFork = OrderFailedInFork(Position(0) / BranchId.Lock % 0 / BranchId.try_(0) % 0 / BranchId.fork("🥕") % 0)
      assert(liveEventSource.nextEvents(aChild.id) == Seq(aChild.id <-: orderFailedInFork))
      aChild = aChild.applyEvent(orderFailedInFork).orThrow

      assert(liveEventSource.nextEvents(bChild.id) == Seq(
        bChild.id <-: OrderLockReleased(LockPath("LOCK-2")),
        bChild.id <-: OrderLockReleased(LockPath("LOCK-1")),
        bChild.id <-: OrderFailedInFork(
          Position(0) / BranchId.Lock % 0 / BranchId.try_(0) % 0 / BranchId.fork("🍋") % 0)))
    }
  }
}

object OrderEventSourceTest
{
  private val TestWorkflowId = WorkflowPath("WORKFLOW") ~ "VERSION"
  private val ForkWorkflow = ForkTestSetting.TestWorkflow.withId(TestWorkflowId)
  private val TestAgentPath = AgentPath("AGENT")
  private val succeededOrderId = OrderId("SUCCESS")
  private val succeededOrder = Order(succeededOrderId, TestWorkflowId, Order.Processed,
    historicOutcomes = Vector(HistoricOutcome(Position(0), Outcome.Succeeded(NamedValues.rc(0)))))
  private val failedOrder = Order(OrderId("FAILED"), TestWorkflowId, Order.Processed,
    historicOutcomes = Vector(HistoricOutcome(Position(0), Outcome.Failed(NamedValues.rc(1)))))
  private val disruptedOrder = Order(OrderId("DISRUPTED"), TestWorkflowId /: Position(2), Order.Processed,
    historicOutcomes = Vector(HistoricOutcome(Position(0), Outcome.Disrupted(Outcome.Disrupted.JobSchedulerRestarted))))
  private val orderForked = OrderForked(Vector(
    OrderForked.Child("🥕", OrderId("ORDER|🥕")),
    OrderForked.Child("🍋", OrderId("ORDER|🍋"))))

  private val executeScript = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("executable")))

  private def step(workflow: Workflow, outcome: Outcome): Seq[OrderEvent] = {
    val process = new SingleOrderProcess(workflow)
    process.update(OrderAdded(workflow.id))
    process.transferToAgent(TestAgentPath)
    process.update(OrderStarted)
    process.jobStep(outcome = outcome)
    process.step()
  }

  final class SingleOrderProcess(val workflow: Workflow, val orderId: OrderId = OrderId("ORDER")) {
    private val process = new Process(workflow)

    def transferToAgent(agentPath: AgentPath) = {
      update(OrderAttachable(agentPath))
      update(OrderAttached(agentPath))
    }

    def transferToController() = {
      update(OrderDetachable)
      update(OrderDetached)
    }

    def jobStep(outcome: Outcome = Outcome.Succeeded(NamedValues.rc(0))) =
      process.jobStep(orderId, outcome)

    def step(): Seq[OrderEvent] =
      process.step(orderId).map(_.event)

    def update(event: OrderEvent) = process.update(orderId <-: event)
  }

  final class Process(workflow: Workflow) {
    val idToWorkflow = Map(workflow.id -> workflow)
    val idToOrder = mutable.Map[OrderId, Order[Order.State]]()
    private val eventHandler = new OrderEventHandler(idToWorkflow.checked, o => idToOrder.checked(o))
    private val inProcess = mutable.Set.empty[OrderId]

    private def eventSource(isAgent: Boolean) =
      new OrderEventSource(StateView.forTest(
        isAgent = isAgent,
        idToOrder = idToOrder.toMap,
        idToWorkflow = idToWorkflow))

    def jobStep(orderId: OrderId, outcome: Outcome = Outcome.succeeded): Unit = {
      update(orderId <-: OrderProcessingStarted)
      update(orderId <-: OrderProcessed(outcome))
    }

    def run(orderId: OrderId): List[KeyedEvent[OrderEvent]] =
      step(orderId) match {
        case keyedEvents if keyedEvents.nonEmpty =>
          keyedEvents.toList ::: (if (idToOrder contains orderId) run(orderId) else Nil)
        case _ => Nil
      }

    def step(orderId: OrderId): Seq[KeyedEvent[OrderEvent]] = {
      val keyedEvents = nextEvents(orderId)
      keyedEvents foreach update
      keyedEvents
    }

    private def nextEvents(orderId: OrderId): Seq[KeyedEvent[OrderEvent]] = {
      val order = idToOrder(orderId)
      if (order.detaching.isRight)
        Seq(order.id <-: OrderDetached)
      else
        (order.state, workflow.instruction(order.position)) match {
          case (_: Order.Ready, _: Execute) =>
            if (order.isDetached)
              Seq(order.id <-: OrderAttachable(TestAgentPath))
            else if (order.isAttaching)
              Seq(order.id <-: OrderAttached(TestAgentPath))
            else
              Seq(order.id <-: OrderProcessingStarted)

          case _ if inProcess contains orderId =>
            Seq(orderId <-: OrderProcessed(Outcome.succeededRC0))

          case _ =>
            eventSource(isAgent = order.isAttached).nextEvents(orderId)
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
            idToOrder(orderId) = idToOrder(orderId).applyEvent(event).orThrow
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

            case FollowUp.Delete(deleteOrderId) =>
              idToOrder -= deleteOrderId

            case o => sys.error(o.toString)
          }
      }
  }

  private def newWorkflowEventSource(
    workflow: Workflow,
    orders: Iterable[Order[Order.State]],
    isAgent: Boolean) =
    new OrderEventSource(StateView.forTest(
      isAgent = isAgent,
      idToOrder = orders.toKeyedMap(_.id),
      idToWorkflow = Map(TestWorkflowId -> workflow)))
}
