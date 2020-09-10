package js7.data.execution.workflow

import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.utils.Collections.implicits._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.Problems.CancelStartedOrderProblem
import js7.data.agent.AgentRefPath
import js7.data.command.CancelMode
import js7.data.event.{<-:, KeyedEvent}
import js7.data.execution.workflow.OrderEventHandler.FollowUp
import js7.data.execution.workflow.OrderEventSourceTest._
import js7.data.expression.Expression.{Equal, LastReturnCode, NumericConstant}
import js7.data.job.{ExecutablePath, ReturnCode}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderCancelMarked, OrderCancelled, OrderCatched, OrderCoreEvent, OrderDetachable, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderResumeMarked, OrderResumed, OrderStarted, OrderSuspendMarked, OrderSuspended, OrderAttached, OrderDetached}
import js7.data.order.{HistoricOutcome, Order, OrderEvent, OrderId, OrderMark, Outcome}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ExplicitEnd, Gap, Goto, If, IfFailedGoto, TryInstruction}
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.position.BranchId.{Else, Then, catch_, try_}
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.test.ForkTestSetting
import js7.data.workflow.{Workflow, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class OrderEventSourceTest extends AnyFreeSpec
{
  "JobSchedulerRestarted" in {
    val eventSource = new OrderEventSource(
      Map(TestWorkflowId -> ForkWorkflow).checked,
      Map(disruptedOrder.id -> disruptedOrder).checked,
      isAgent = false)
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
      process.transferToController()
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
    process.update(orderId <-: OrderAttached(TestAgentRefPath))
    assert(process.run(orderId) == List(
      orderId <-: OrderStarted,
      orderId <-: OrderForked(List(
        OrderForked.Child("🥕", orderId / "🥕"),
        OrderForked.Child("🍋", orderId / "🍋"))),
      orderId <-: OrderDetachable,
      orderId <-: OrderDetached))

    assert(process.run(orderId / "🥕") == List(
      orderId / "🥕" <-: OrderProcessingStarted,
      orderId / "🥕" <-: OrderProcessed(Outcome.succeeded),
      orderId / "🥕" <-: OrderMoved(Position(0) / "fork+🥕" % 1),
      orderId / "🥕" <-: OrderDetachable,
      orderId / "🥕" <-: OrderDetached))

    assert(process.step(orderId).isEmpty)  // Nothing to join

    assert(process.run(orderId / "🍋") == List(
      orderId / "🍋" <-: OrderProcessingStarted,
      orderId / "🍋" <-: OrderProcessed(Outcome.succeeded),
      orderId / "🍋" <-: OrderMoved(Position(0) / "fork+🍋" % 1),
      orderId / "🍋" <-: OrderDetachable,
      orderId / "🍋" <-: OrderDetached,
      orderId <-: OrderJoined(Outcome.succeeded)))
    assert(process.step(orderId) == Some(orderId <-: OrderMoved(Position(1))))

    assert(process.step(orderId) == Some(orderId <-: OrderForked(List(
      OrderForked.Child("🥕", orderId / "🥕"),
      OrderForked.Child("🍋", orderId / "🍋")))))

    assert(process.run(orderId / "🥕") == List(
      orderId / "🥕" <-: OrderAttachable(TestAgentRefPath),
      orderId / "🥕" <-: OrderAttached(TestAgentRefPath),
      orderId / "🥕" <-: OrderProcessingStarted,
      orderId / "🥕" <-: OrderProcessed(Outcome.succeeded),
      orderId / "🥕" <-: OrderMoved(Position(1) / "fork+🥕" % 1),
      orderId / "🥕" <-: OrderDetachable,
      orderId / "🥕" <-: OrderDetached))

    assert(process.step(orderId).isEmpty)  // Nothing to join

    assert(process.run(orderId / "🍋") == List(
      orderId / "🍋" <-: OrderAttachable(TestAgentRefPath),
      orderId / "🍋" <-: OrderAttached(TestAgentRefPath),
      orderId / "🍋" <-: OrderProcessingStarted,
      orderId / "🍋" <-: OrderProcessed(Outcome.succeeded),
      orderId / "🍋" <-: OrderMoved(Position(1) / "fork+🍋" % 1),
      orderId / "🍋" <-: OrderDetachable,
      orderId / "🍋" <-: OrderDetached,
      orderId <-: OrderJoined(Outcome.succeeded)))

    assert(process.step(orderId) == Some(orderId <-: OrderMoved(Position(2))))
    assert(process.step(orderId) == Some(orderId <-: OrderAttachable(TestAgentRefPath)))
    assert(process.step(orderId) == Some(orderId <-: OrderAttached(TestAgentRefPath)))
    assert(process.step(orderId) == Some(orderId <-: OrderProcessingStarted))
    // and so forth...
  }

  "applyMoveInstructions" - {
    for (isAgent <- Seq(false, true)) s"isAgent=$isAgent" - {
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
    "Order.mark.isEmpty" - {
      val freshOrder = Order(OrderId("ORDER"), TestWorkflowId, Order.Fresh(None))
      "Fresh" - {
        "Detached" in {
          val order = freshOrder
          val eventSource = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = false)
          assert(eventSource.nextEvent(order.id) == Some(order.id <-: OrderStarted))
          assert(eventSource.cancel(order.id, CancelMode.NotStarted      ) == Right(Some(OrderCancelled)))
          assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelled)))
          assert(eventSource.suspend(order.id) == Right(Some(OrderSuspended)))
          assert(eventSource.resume(order.id) == Left(Problem("Order cannot resume because it is not suspended")))
        }

        "Attaching" in {
          val order = freshOrder.copy(attachedState = Some(Order.Attaching(TestAgentRefPath)))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == None)
          assert(eventSource(isAgent = true ).nextEvent(order.id) == None)
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Right(Some(OrderCancelMarked(CancelMode.NotStarted))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Right(Some(OrderCancelMarked(CancelMode.NotStarted))))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(eventSource(isAgent = false).suspend(order.id) == Right(Some(OrderSuspendMarked)))
          assert(eventSource(isAgent = true ).suspend(order.id) == Right(Some(OrderSuspendMarked)))
          assert(eventSource(isAgent = false).resume(order.id) == Left(Problem("Order cannot resume because it is not suspended")))
          assert(eventSource(isAgent = true ).resume(order.id) == Left(Problem("Order cannot resume because it is not suspended")))
        }

        "Attached" in {
          val order = freshOrder.copy(attachedState = Some(Order.Attached(TestAgentRefPath)))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == None)
          assert(eventSource(isAgent = true ).nextEvent(order.id) == Some(order.id <-: OrderStarted))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Right(Some(OrderCancelMarked(CancelMode.NotStarted))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Right(Some(OrderDetachable)))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderDetachable)))
          assert(eventSource(isAgent = true ).suspend(order.id) == Right(Some(OrderDetachable)))
          assert(eventSource(isAgent = false).suspend(order.id) == Right(Some(OrderSuspendMarked)))
          assert(eventSource(isAgent = true ).resume(order.id) == Left(Problem("Order cannot resume because it is not suspended")))
          assert(eventSource(isAgent = false).resume(order.id) == Left(Problem("Order cannot resume because it is not suspended")))
        }

        "Detaching" in {
          val order = freshOrder.copy(attachedState = Some(Order.Detaching(TestAgentRefPath)))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == None)
          assert(eventSource(isAgent = true ).nextEvent(order.id) == None)
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Right(Some(OrderCancelMarked(CancelMode.NotStarted))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Right(Some(OrderCancelMarked(CancelMode.NotStarted))))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
        }
      }

      "Ready" - {
        val readyOrder = freshOrder.copy(state = Order.Ready)
        "Detached" in {
          val order = readyOrder
          val eventSource = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = false)
          assert(eventSource.nextEvent(order.id) == Some(order.id <-: orderForked))
          assert(eventSource.cancel(order.id, CancelMode.NotStarted    ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelled)))
        }

        "Attaching" in {
          val order = readyOrder.copy(attachedState = Some(Order.Attaching(TestAgentRefPath)))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == None)
          assert(eventSource(isAgent = true ).nextEvent(order.id) == None)
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
        }

        "Attached" in {
          val order = readyOrder.copy(attachedState = Some(Order.Attached(TestAgentRefPath)))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == None)
          assert(eventSource(isAgent = true ).nextEvent(order.id) == Some(order.id <-: orderForked))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderDetachable)))
        }

        "Detaching" in {
          val order = readyOrder.copy(attachedState = Some(Order.Detaching(TestAgentRefPath)))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == None)
          assert(eventSource(isAgent = true ).nextEvent(order.id) == None)
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
        }
      }

      "Processing Attached" in {
        val order = freshOrder.copy(state = Order.Processing, attachedState = Some(Order.Attached(TestAgentRefPath)))
        def eventSource(isAgent: Boolean) = new OrderEventSource(
          Map(TestWorkflowId -> ForkWorkflow).checked,
          Map(order.id -> order).checked,
          isAgent = isAgent)
        assert(eventSource(isAgent = false).nextEvent(order.id) == None)
        assert(eventSource(isAgent = true ).nextEvent(order.id) == None)
        assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
        assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
      }
    }

    "Order.isSuspended" - {
      val suspendedOrder = Order(OrderId("ORDER"), TestWorkflowId, Order.Fresh(None), isSuspended = true)
      "Fresh" - {
        "Detached" in {
          val order = suspendedOrder
          val eventSource = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = false)
          assert(eventSource.nextEvent(order.id) == Some(order.id <-: OrderStarted))
          assert(eventSource.cancel(order.id, CancelMode.NotStarted      ) == Right(Some(OrderCancelled)))
          assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelled)))
          assert(eventSource.suspend(order.id) == Right(None))
          assert(eventSource.resume(order.id) == Right(Some(OrderResumed)))
        }

        "Attaching" in {
          val order = suspendedOrder.copy(attachedState = Some(Order.Attaching(TestAgentRefPath)))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == None)
          assert(eventSource(isAgent = true ).nextEvent(order.id) == None)
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Right(Some(OrderCancelMarked(CancelMode.NotStarted))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Right(Some(OrderCancelMarked(CancelMode.NotStarted))))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(eventSource(isAgent = false).suspend(order.id) == Right(None))
          assert(eventSource(isAgent = true ).suspend(order.id) == Right(None))
          assert(eventSource(isAgent = false).resume(order.id) == Right(Some(OrderResumeMarked)))
          assert(eventSource(isAgent = true ).resume(order.id) == Right(Some(OrderResumeMarked)))
        }

        "Attached" in {
          val order = suspendedOrder.copy(attachedState = Some(Order.Attached(TestAgentRefPath)))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == None)
          assert(eventSource(isAgent = true ).nextEvent(order.id) == Some(order.id <-: OrderStarted))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Right(Some(OrderCancelMarked(CancelMode.NotStarted))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Right(Some(OrderDetachable)))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderDetachable)))
          assert(eventSource(isAgent = false).suspend(order.id) == Right(None))
          assert(eventSource(isAgent = true ).suspend(order.id) == Right(None))
          assert(eventSource(isAgent = false).resume(order.id) == Right(Some(OrderResumeMarked)))
          assert(eventSource(isAgent = true ).resume(order.id) == Right(Some(OrderResumed)))
        }

        "Detaching" in {
          val order = suspendedOrder.copy(attachedState = Some(Order.Detaching(TestAgentRefPath)))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == None)
          assert(eventSource(isAgent = true ).nextEvent(order.id) == None)
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Right(Some(OrderCancelMarked(CancelMode.NotStarted))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Right(Some(OrderCancelMarked(CancelMode.NotStarted))))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
        }
      }

      "Ready" - {
        val readyOrder = suspendedOrder.copy(state = Order.Ready)
        "Detached" in {
          val order = readyOrder
          val eventSource = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = false)
          assert(eventSource.nextEvent(order.id) == Some(order.id <-: orderForked))
          assert(eventSource.cancel(order.id, CancelMode.NotStarted    ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelled)))
        }

        "Attaching" in {
          val order = readyOrder.copy(attachedState = Some(Order.Attaching(TestAgentRefPath)))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == None)
          assert(eventSource(isAgent = true ).nextEvent(order.id) == None)
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
        }

        "Attached" in {
          val order = readyOrder.copy(attachedState = Some(Order.Attached(TestAgentRefPath)))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == None)
          assert(eventSource(isAgent = true ).nextEvent(order.id) == Some(order.id <-: orderForked))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderDetachable)))
        }

        "Detaching" in {
          val order = readyOrder.copy(attachedState = Some(Order.Detaching(TestAgentRefPath)))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == None)
          assert(eventSource(isAgent = true ).nextEvent(order.id) == None)
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
        }
      }

      "Processing Attached" in {
        val order = suspendedOrder.copy(state = Order.Processing, attachedState = Some(Order.Attached(TestAgentRefPath)))
        def eventSource(isAgent: Boolean) = new OrderEventSource(
          Map(TestWorkflowId -> ForkWorkflow).checked,
          Map(order.id -> order).checked,
          isAgent = isAgent)
        assert(eventSource(isAgent = false).nextEvent(order.id) == None)
        assert(eventSource(isAgent = true ).nextEvent(order.id) == None)
        assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
        assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
      }
    }
  }

  "OrderMark" - {
    "OrderMark.Cancelling(NotStarted)" - {
      "Ready" - {
        "Detached" in {
          val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready,
            mark = Some(OrderMark.Cancelling(CancelMode.NotStarted)))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == Some(order.id <-: orderForked))
        }

        "Attached" in {
          val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready, attachedState = Some(Order.Attached(TestAgentRefPath)),
            mark = Some(OrderMark.Cancelling(CancelMode.FreshOrStarted())))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == None)
          assert(eventSource(isAgent = true ).nextEvent(order.id) == Some(order.id <-: OrderDetachable))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(None))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderDetachable)))
        }
      }

      "Processing Attached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Processing, attachedState = Some(Order.Attached(TestAgentRefPath)),
          mark = Some(OrderMark.Cancelling(CancelMode.FreshOrStarted())))
        def eventSource(isAgent: Boolean) = new OrderEventSource(
          Map(TestWorkflowId -> ForkWorkflow).checked,
          Map(order.id -> order).checked,
          isAgent = isAgent)
        assert(eventSource(isAgent = false).nextEvent(order.id) == None)
        assert(eventSource(isAgent = true ).nextEvent(order.id) == None)
        assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(None))
        assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(None))
      }
    }

    "OrderMark.Cancelling(FreshOrStarted)" - {
      "Ready" - {
        "Detached" in {
          val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready,
            mark = Some(OrderMark.Cancelling(CancelMode.FreshOrStarted())))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == Some(order.id <-: OrderCancelled))
        }

        "Attached" in {
          val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready, attachedState = Some(Order.Attached(TestAgentRefPath)),
            mark = Some(OrderMark.Cancelling(CancelMode.FreshOrStarted())))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == None)
          assert(eventSource(isAgent = true ).nextEvent(order.id) == Some(order.id <-: OrderDetachable))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
          assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(None))
          assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderDetachable)))
        }
      }

      "Processing Attached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Processing, attachedState = Some(Order.Attached(TestAgentRefPath)),
          mark = Some(OrderMark.Cancelling(CancelMode.FreshOrStarted())))
        def eventSource(isAgent: Boolean) = new OrderEventSource(
          Map(TestWorkflowId -> ForkWorkflow).checked,
          Map(order.id -> order).checked,
          isAgent = isAgent)
        assert(eventSource(isAgent = false).nextEvent(order.id) == None)
        assert(eventSource(isAgent = true ).nextEvent(order.id) == None)
        assert(eventSource(isAgent = false).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.NotStarted      ) == Left(CancelStartedOrderProblem(order.id)))
        assert(eventSource(isAgent = false).cancel(order.id, CancelMode.FreshOrStarted()) == Right(None))
        assert(eventSource(isAgent = true ).cancel(order.id, CancelMode.FreshOrStarted()) == Right(None))
      }
    }

    "OrderMark.Suspending" - {
      "Ready" - {
        "Detached" in {
          val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready,
            mark = Some(OrderMark.Suspending))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == Some(order.id <-: OrderSuspended))
        }

        "Attached" in {
          val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready, attachedState = Some(Order.Attached(TestAgentRefPath)),
            mark = Some(OrderMark.Suspending))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == None)
          assert(eventSource(isAgent = true ).nextEvent(order.id) == Some(order.id <-: OrderDetachable))
        }
      }

      "Processing Attached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Processing, attachedState = Some(Order.Attached(TestAgentRefPath)),
          mark = Some(OrderMark.Suspending))
        def eventSource(isAgent: Boolean) = new OrderEventSource(
          Map(TestWorkflowId -> ForkWorkflow).checked,
          Map(order.id -> order).checked,
          isAgent = isAgent)
        assert(eventSource(isAgent = false).nextEvent(order.id) == None)
        assert(eventSource(isAgent = true ).nextEvent(order.id) == None)
      }
    }

    "OrderMark.Resuming" - {
      "Ready" - {
        "Detached" in {
          val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready,
            isSuspended = true, mark = Some(OrderMark.Resuming))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == Some(order.id <-: OrderResumed))
        }

        "Attached" in {
          val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready, attachedState = Some(Order.Attached(TestAgentRefPath)),
            isSuspended = true, mark = Some(OrderMark.Resuming))
          def eventSource(isAgent: Boolean) = new OrderEventSource(
            Map(TestWorkflowId -> ForkWorkflow).checked,
            Map(order.id -> order).checked,
            isAgent = isAgent)
          assert(eventSource(isAgent = false).nextEvent(order.id) == None)
          assert(eventSource(isAgent = true ).nextEvent(order.id) == Some(order.id <-: OrderResumed))
        }
      }

      "Processing Attached" in {
        val order = Order(OrderId("ORDER"), TestWorkflowId, Order.Processing, attachedState = Some(Order.Attached(TestAgentRefPath)),
          mark = Some(OrderMark.Resuming))
        def eventSource(isAgent: Boolean) = new OrderEventSource(
          Map(TestWorkflowId -> ForkWorkflow).checked,
          Map(order.id -> order).checked,
          isAgent = isAgent)
        assert(eventSource(isAgent = false).nextEvent(order.id) == None)
        assert(eventSource(isAgent = true ).nextEvent(order.id) == None)
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
      new OrderEventSource(
        Map(workflow.id -> workflow).checked,
        Map(order.id -> order).checked,
        isAgent = false)
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

    "Processed failed in outer catch-block -> OrderFailed" in {
      val pos = Position(0) / catch_(0) % 0
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = HistoricOutcome(pos, failed) :: Nil)
      assert(eventSource(order).nextEvent(order.id) == Some(order.id <-:
        OrderFailed(failed)))
    }

    "Processed failed in try in catch -> OrderCatched" in {
      val pos = Position(0) / catch_(0) % 1 / try_(0) % 0
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = HistoricOutcome(pos, failed) :: Nil)
      assert(eventSource(order).nextEvent(order.id) == Some(order.id <-:
        OrderCatched(failed, Position(0) / catch_(0) % 1 / catch_(0) % 0)))
    }

    "Processed failed in catch in catch -> OrderFailed" in {
      val order = Order(OrderId("ORDER"), workflow.id /: (Position(0) / catch_(0) % 0), Order.Processed,
        historicOutcomes = HistoricOutcome(Position(0), failed) :: Nil)
      assert(eventSource(order).nextEvent(order.id) == Some(order.id <-:
        OrderFailed(failed)))
    }

    "Processed failed not in try/catch -> OrderFailed" in {
      val pos = Position(1)
      val order = Order(OrderId("ORDER"), workflow.id /: pos, Order.Processed,
        historicOutcomes = HistoricOutcome(pos, failed) :: Nil)
      assert(eventSource(order).nextEvent(order.id) == Some(order.id <-:
        OrderFailed(failed)))
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
      def liveEventSource = new OrderEventSource(
        Map(workflow.id -> workflow).checked,
        Map(
          forkingOrder.id -> forkingOrder,
          aChild.id -> aChild,
          bChild.id -> bChild
        ).checked,
        isAgent = false)

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
  private val orderForked = OrderForked(List(
    OrderForked.Child("🥕", OrderId("ORDER/🥕")),
    OrderForked.Child("🍋", OrderId("ORDER/🍋"))))

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
      update(OrderAttached(agentRefPath))
    }

    def transferToController() = {
      update(OrderDetachable)
      update(OrderDetached)
    }

    def jobStep(outcome: Outcome = Outcome.Succeeded(ReturnCode.Success)) =
      process.jobStep(orderId, outcome)

    def step(): Option[OrderEvent] =
      process.step(orderId) map (_.event)

    def update(event: OrderEvent) = process.update(orderId <-: event)
  }

  final class Process(workflow: Workflow) {
    val idToWorkflow = Map(workflow.id -> workflow).checked(_)
    val idToOrder = mutable.Map[OrderId, Order[Order.State]]()
    private def eventSource(isAgent: Boolean) = new OrderEventSource(idToWorkflow, o => idToOrder.checked(o), isAgent = isAgent)
    private val eventHandler = new OrderEventHandler(idToWorkflow, o => idToOrder.checked(o))
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
        Some(order.id <-: OrderDetached)
      else
        (order.state, workflow.instruction(order.position)) match {
          case (_: Order.Ready, _: Execute) =>
            if (order.isDetached)
              Some(order.id <-: OrderAttachable(TestAgentRefPath))
            else if (order.isAttaching)
              Some(order.id <-: OrderAttached(TestAgentRefPath))
            else
              Some(order.id <-: OrderProcessingStarted)

          case _ if inProcess contains orderId =>
            Some(orderId <-: OrderProcessed(Outcome.succeeded))

          case _ =>
            eventSource(isAgent = order.isAttached).nextEvent(orderId)
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

  private def newWorkflowEventSource(workflow: Workflow, orders: Iterable[Order[Order.State]], isAgent: Boolean) =
    new OrderEventSource(
      Map(TestWorkflowId -> workflow).checked,
      orders.toKeyedMap(_.id).checked,
      isAgent = isAgent)
}
