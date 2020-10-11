package js7.data.execution.workflow

import cats.syntax.option._
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.utils.Collections.implicits._
import js7.base.utils.ScalaUtils.syntax._
import js7.data.Problems.CancelStartedOrderProblem
import js7.data.agent.AgentName
import js7.data.command.CancelMode
import js7.data.event.{<-:, KeyedEvent}
import js7.data.execution.workflow.OrderEventHandler.FollowUp
import js7.data.execution.workflow.OrderEventSourceTest._
import js7.data.expression.Expression.{Equal, LastReturnCode, NumericConstant}
import js7.data.job.{ExecutablePath, ReturnCode}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelMarked, OrderCancelled, OrderCatched, OrderCoreEvent, OrderDetachable, OrderDetached, OrderFailed, OrderFailedInFork, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderResumeMarked, OrderResumed, OrderStarted, OrderSuspendMarked, OrderSuspended}
import js7.data.order.{HistoricOutcome, Order, OrderEvent, OrderId, OrderMark, Outcome}
import js7.data.problems.{CannotResumeOrderProblem, CannotSuspendOrderProblem}
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
      process.transferToAgent(TestAgentName)
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
    process.update(orderId <-: OrderAttachable(TestAgentName))
    process.update(orderId <-: OrderAttached(TestAgentName))
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
      orderId / "🥕" <-: OrderAttachable(TestAgentName),
      orderId / "🥕" <-: OrderAttached(TestAgentName),
      orderId / "🥕" <-: OrderProcessingStarted,
      orderId / "🥕" <-: OrderProcessed(Outcome.succeeded),
      orderId / "🥕" <-: OrderMoved(Position(1) / "fork+🥕" % 1),
      orderId / "🥕" <-: OrderDetachable,
      orderId / "🥕" <-: OrderDetached))

    assert(process.step(orderId).isEmpty)  // Nothing to join

    assert(process.run(orderId / "🍋") == List(
      orderId / "🍋" <-: OrderAttachable(TestAgentName),
      orderId / "🍋" <-: OrderAttached(TestAgentName),
      orderId / "🍋" <-: OrderProcessingStarted,
      orderId / "🍋" <-: OrderProcessed(Outcome.succeeded),
      orderId / "🍋" <-: OrderMoved(Position(1) / "fork+🍋" % 1),
      orderId / "🍋" <-: OrderDetachable,
      orderId / "🍋" <-: OrderDetached,
      orderId <-: OrderJoined(Outcome.succeeded)))

    assert(process.step(orderId) == Some(orderId <-: OrderMoved(Position(2))))
    assert(process.step(orderId) == Some(orderId <-: OrderAttachable(TestAgentName)))
    assert(process.step(orderId) == Some(orderId <-: OrderAttached(TestAgentName)))
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
    val detached = none[Order.AttachedState]
    val attaching = Some(Order.Attaching(TestAgentName))
    val attached = Some(Order.Attached(TestAgentName))
    val detaching = Some(Order.Detaching(TestAgentName))

    "Order.mark.isEmpty" - {
      val unmarkedOrder = Order(OrderId("ORDER"), TestWorkflowId, Order.Fresh(None))

      "Fresh" - {
        val freshOrder = unmarkedOrder

        "Detached" in {
          testEventSource(freshOrder, detached) { (order, controller, _) =>
            assert(controller.nextEvent(order.id) == Some(order.id <-: OrderStarted))
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Right(Some(OrderCancelled)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelled)))
            assert(controller.suspend(order.id) == Right(Some(OrderSuspended)))
            assert(controller.resume(order.id, None) == Left(CannotResumeOrderProblem))
          }
        }

        "Attaching" in {
          testEventSource(freshOrder, attaching) { (order, controller, agent) =>
            assert(controller.nextEvent(order.id) == None)
            assert(agent     .nextEvent(order.id) == None)
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Right(Some(OrderCancelMarked(CancelMode.FreshOnly))))
            assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Right(Some(OrderCancelMarked(CancelMode.FreshOnly))))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(controller.suspend(order.id) == Right(Some(OrderSuspendMarked)))
            assert(agent     .suspend(order.id) == Right(Some(OrderSuspendMarked)))
            assert(controller.resume(order.id, None) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None) == Left(CannotResumeOrderProblem))
          }
        }

        "Attached" in {
          testEventSource(freshOrder, attached) { (order, controller, agent) =>
            assert(controller.nextEvent(order.id) == None)
            assert(agent     .nextEvent(order.id) == Some(order.id <-: OrderStarted))
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Right(Some(OrderCancelMarked(CancelMode.FreshOnly))))
            assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Right(Some(OrderDetachable)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderDetachable)))
            assert(controller.suspend(order.id) == Right(Some(OrderSuspendMarked)))
            assert(agent     .suspend(order.id) == Right(Some(OrderDetachable)))
            assert(controller.resume(order.id, None) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None) == Left(CannotResumeOrderProblem))
          }
        }

        "Detaching" in {
          testEventSource(freshOrder, detaching) { (order, controller, agent) =>
            assert(controller.nextEvent(order.id) == None)
            assert(agent     .nextEvent(order.id) == None)
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Right(Some(OrderCancelMarked(CancelMode.FreshOnly))))
            assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Right(Some(OrderCancelMarked(CancelMode.FreshOnly))))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(controller.suspend(order.id) == Right(Some(OrderSuspendMarked)))
            assert(agent     .suspend(order.id) == Right(Some(OrderSuspendMarked)))
            assert(controller.resume(order.id, None) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None) == Left(CannotResumeOrderProblem))
          }
        }
      }

      "Ready" - {
        val readyOrder = unmarkedOrder.copy(state = Order.Ready)

        "Detached" in {
          testEventSource(readyOrder, attachedState = detached) { (order, controller, _) =>
            assert(controller.nextEvent(order.id) == Some(order.id <-: orderForked))
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelled)))
          }
        }

        "Attaching" in {
          testEventSource(readyOrder, attaching) { (order, controller, agent) =>
            assert(controller.nextEvent(order.id) == None)
            assert(agent     .nextEvent(order.id) == None)
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(controller.suspend(order.id) == Right(Some(OrderSuspendMarked)))
            assert(agent     .suspend(order.id) == Right(Some(OrderSuspendMarked)))
            assert(controller.resume(order.id, None) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None) == Left(CannotResumeOrderProblem))
          }
        }

        "Attached" in {
          testEventSource(readyOrder, attached) { (order, controller, agent) =>
            assert(controller.nextEvent(order.id) == None)
            assert(agent     .nextEvent(order.id) == Some(order.id <-: orderForked))
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderDetachable)))
            assert(controller.suspend(order.id) == Right(Some(OrderSuspendMarked)))
            assert(agent     .suspend(order.id) == Right(Some(OrderDetachable)))
            assert(controller.resume(order.id, None) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None) == Left(CannotResumeOrderProblem))
          }
        }

        "Detaching" in {
          testEventSource(readyOrder, detaching) { (order, controller, agent) =>
            assert(controller.nextEvent(order.id) == None)
            assert(agent     .nextEvent(order.id) == None)
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(controller.suspend(order.id) == Right(Some(OrderSuspendMarked)))
            assert(agent     .suspend(order.id) == Right(Some(OrderSuspendMarked)))
            assert(controller.resume(order.id, None) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None) == Left(CannotResumeOrderProblem))
          }
        }
      }

      "Processing Attached" in {
        val processingOrder = unmarkedOrder.copy(state = Order.Processing)
        testEventSource(processingOrder, attached) { (order, controller, agent) =>
          assert(controller.nextEvent(order.id) == None)
          assert(agent     .nextEvent(order.id) == None)
          assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
          assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
          assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(controller.suspend(order.id) == Right(Some(OrderSuspendMarked)))
          assert(agent     .suspend(order.id) == Right(Some(OrderSuspendMarked)))
          assert(controller.resume(order.id, None) == Left(CannotResumeOrderProblem))
          assert(agent     .resume(order.id, None) == Left(CannotResumeOrderProblem))
        }
      }
    }

    "OrderMark.Cancelling(FreshOnly)" - {
      val cancellingOrder = Order(OrderId("ORDER"), TestWorkflowId, Order.Fresh(None), mark = Some(OrderMark.Cancelling(CancelMode.FreshOnly)))

      "Fresh" - {
        val freshOrder = cancellingOrder

        "Detached" in {
          testEventSource(freshOrder, detached) { (order, controller, _) =>
            assert(controller.nextEvent(order.id) == Some(order.id <-: OrderCancelled))
          }
        }

        "Attached" in {
          testEventSource(freshOrder, attached) { (order, controller, agent) =>
            assert(controller.nextEvent(order.id) == None)
            assert(agent     .nextEvent(order.id) == Some(order.id <-: OrderDetachable))
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Right(None))
            assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Right(Some(OrderDetachable)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(None))
            assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderDetachable)))
            assert(controller.suspend(order.id) == Left(CannotSuspendOrderProblem))
            assert(agent     .suspend(order.id) == Left(CannotSuspendOrderProblem))
            assert(controller.resume(order.id, None) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None) == Left(CannotResumeOrderProblem))
            assert(controller.resume(order.id, Some(Position(1))) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, Some(Position(1))) == Left(CannotResumeOrderProblem))
          }
        }
      }
    }

    "OrderMark.Cancelling(FreshOrStarted)" - {
      val cancellingOrder = Order(OrderId("ORDER"), TestWorkflowId, Order.Fresh(None), mark = Some(OrderMark.Cancelling(CancelMode.FreshOnly)))

      "Ready" - {
        val readyOrder = cancellingOrder.copy(state = Order.Ready)

        "Detached" in {
          testEventSource(readyOrder, detached) { (order, controller, _) =>
            assert(controller.nextEvent(order.id) == Some(order.id <-: orderForked))
          }
        }

        "Attached" in {
          testEventSource(readyOrder, attached) { (order, controller, agent) =>
            assert(controller.nextEvent(order.id) == None)
            assert(agent     .nextEvent(order.id) == Some(order.id <-: orderForked))
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(None))
            assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderDetachable)))
            assert(controller.suspend(order.id) == Left(CannotSuspendOrderProblem))
            assert(agent     .suspend(order.id) == Left(CannotSuspendOrderProblem))
            assert(controller.resume(order.id, None) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, None) == Left(CannotResumeOrderProblem))
            assert(controller.resume(order.id, Some(Position(1))) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, Some(Position(1))) == Left(CannotResumeOrderProblem))
          }
        }
      }

      "Processing Attached" in {
        val processingOrder = cancellingOrder.copy(state = Order.Processing)
        testEventSource(processingOrder, attached) { (order, controller, agent) =>
          assert(controller.nextEvent(order.id) == None)
          assert(agent     .nextEvent(order.id) == None)
          assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
          assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
          assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(None))
          assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(None))
          assert(controller.suspend(order.id) == Left(CannotSuspendOrderProblem))
          assert(agent     .suspend(order.id) == Left(CannotSuspendOrderProblem))
          assert(controller.resume(order.id, None) == Left(CannotResumeOrderProblem))
          assert(agent     .resume(order.id, None) == Left(CannotResumeOrderProblem))
          assert(controller.resume(order.id, Some(Position(1))) == Left(CannotResumeOrderProblem))
          assert(agent     .resume(order.id, Some(Position(1))) == Left(CannotResumeOrderProblem))
        }
      }
    }

    "OrderMark.Suspending" - {
      val suspendingOrder = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready, mark = Some(OrderMark.Suspending))

      "Ready" - {
        val readyOrder = suspendingOrder

        "Detached" in {
          testEventSource(readyOrder, detached) { (order, controller, _) =>
            assert(controller.nextEvent(order.id) == Some(order.id <-: OrderSuspended))
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelled)))
            assert(controller.suspend(order.id) == Right(None))
            assert(controller.resume(order.id, None) == Right(Some(OrderResumeMarked())))
            assert(controller.resume(order.id, Some(Position(1))) == Left(CannotResumeOrderProblem))
          }
        }

        "Attached" in {
          testEventSource(readyOrder, attached) { (order, controller, agent) =>
            assert(controller.nextEvent(order.id) == None)
            assert(agent     .nextEvent(order.id) == Some(order.id <-: OrderDetachable))
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderDetachable)))
            assert(controller.suspend(order.id) == Right(None))
            assert(agent     .suspend(order.id) == Right(None))
            assert(controller.resume(order.id, None) == Right(Some(OrderResumeMarked())))
            assert(agent     .resume(order.id, None) == Right(Some(OrderResumeMarked())))
            assert(controller.resume(order.id, Some(Position(1))) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, Some(Position(1))) == Left(CannotResumeOrderProblem))
          }
        }
      }

      "Processing Attached" in {
        val processingOrder = suspendingOrder.copy(state = Order.Processing)
        testEventSource(processingOrder, attached) { (order, controller, agent) =>
          assert(controller.nextEvent(order.id) == None)
          assert(agent     .nextEvent(order.id) == None)
          assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
          assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(controller.suspend(order.id) == Right(None))
          assert(agent     .suspend(order.id) == Right(None))
          assert(controller.resume(order.id, None) == Right(Some(OrderResumeMarked())))
          assert(agent     .resume(order.id, None) == Right(Some(OrderResumeMarked())))
          assert(controller.resume(order.id, Some(Position(1))) == Left(CannotResumeOrderProblem))
          assert(agent     .resume(order.id, Some(Position(1))) == Left(CannotResumeOrderProblem))
        }
      }
    }

    "OrderMark.Resuming isSuspended" - {
      val resumingOrder = Order(OrderId("ORDER"), TestWorkflowId, Order.Ready, mark = Some(OrderMark.Resuming()), isSuspended = true)

      "Ready" - {
        val readyOrder = resumingOrder

        "Detached" in {
          testEventSource(readyOrder, detached) { (order, controller, _) =>
            assert(controller.nextEvent(order.id) == Some(order.id <-: OrderResumed()))
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelled)))
            assert(controller.suspend(order.id) == Right(Some(OrderSuspended)))
            assert(controller.resume(order.id, None) == Right(Some(OrderResumed()/*should already be happened*/)))
            assert(controller.resume(order.id, Some(Position(1))) == Left(CannotResumeOrderProblem))
          }
        }

        "Attached" in {
          testEventSource(readyOrder, attached) { (order, controller, agent) =>
            assert(controller.nextEvent(order.id) == None)
            assert(agent     .nextEvent(order.id) == Some(order.id <-: OrderResumed()))
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderDetachable)))
            assert(controller.suspend(order.id) == Right(None))
            assert(agent     .suspend(order.id) == Right(Some(OrderDetachable)))
            assert(controller.resume(order.id, None) == Right(None))
            assert(agent     .resume(order.id, None) == Right(None))
            assert(controller.resume(order.id, Some(Position(1))) == Left(CannotResumeOrderProblem))
            assert(agent     .resume(order.id, Some(Position(1))) == Left(CannotResumeOrderProblem))
          }
        }
      }
    }

    "Order.isSuspended" - {
      val suspendedOrder = Order(OrderId("ORDER"), TestWorkflowId, Order.Fresh(None), isSuspended = true)

      "Fresh" - {
        val freshOrder = suspendedOrder

        "Detached" in {
          testEventSource(freshOrder, detached) { (order, controller, _) =>
            assert(controller.nextEvent(order.id) == None)
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Right(Some(OrderCancelled)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelled)))
            assert(controller.suspend(order.id) == Right(None))
            assert(controller.resume(order.id, None) == Right(Some(OrderResumed())))
            assert(controller.resume(order.id, Some(Position(1))) == Right(Some(OrderResumed(Some(Position(1))))))
            assert(controller.resume(order.id, Some(Position(99))) == Left(Problem("ResumeOrder: position is not reachable")))
          }
        }

        "Attaching" in {
          testEventSource(freshOrder, attaching) { (order, controller, agent) =>
            assert(controller.nextEvent(order.id) == None)
            assert(agent     .nextEvent(order.id) == None)
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Right(Some(OrderCancelMarked(CancelMode.FreshOnly))))
            assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Right(Some(OrderCancelMarked(CancelMode.FreshOnly))))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(controller.suspend(order.id) == Right(None))
            assert(agent     .suspend(order.id) == Right(None))
            assert(controller.resume(order.id, None) == Right(Some(OrderResumeMarked())))
            assert(agent     .resume(order.id, None) == Right(Some(OrderResumeMarked())))
            assert(controller.resume(order.id, Some(Position(1))) == Right(Some(OrderResumeMarked(Some(Position(1))))))
            assert(agent     .resume(order.id, Some(Position(1))) == Right(Some(OrderResumeMarked(Some(Position(1))))))
          }
        }

        "Attached" in {
          testEventSource(freshOrder, attached) { (order, controller, agent) =>
            assert(controller.nextEvent(order.id) == None)
            assert(agent     .nextEvent(order.id) == None)
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Right(Some(OrderCancelMarked(CancelMode.FreshOnly))))
            assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Right(Some(OrderDetachable)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderDetachable)))
            assert(controller.suspend(order.id) == Right(None))
            assert(agent     .suspend(order.id) == Right(Some(OrderDetachable)))
            assert(controller.resume(order.id, None) == Right(Some(OrderResumeMarked())))
            assert(agent     .resume(order.id, None) == Right(Some(OrderResumed())))
            assert(controller.resume(order.id, Some(Position(1))) == Right(Some(OrderResumeMarked(Some(Position(1))))))
            assert(agent     .resume(order.id, Some(Position(1))) == Right(Some(OrderResumed(Some(Position(1))))))
          }
        }

        "Detaching" in {
          testEventSource(freshOrder, detaching) { (order, controller, agent) =>
            assert(controller.nextEvent(order.id) == None)
            assert(agent     .nextEvent(order.id) == None)
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Right(Some(OrderCancelMarked(CancelMode.FreshOnly))))
            assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Right(Some(OrderCancelMarked(CancelMode.FreshOnly))))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(controller.suspend(order.id) == Right(None))
            assert(agent     .suspend(order.id) == Right(None))
            assert(controller.resume(order.id, None) == Right(Some(OrderResumeMarked())))
            assert(agent     .resume(order.id, None) == Right(Some(OrderResumeMarked())))
            assert(controller.resume(order.id, Some(Position(1))) == Right(Some(OrderResumeMarked(Some(Position(1))))))
            assert(agent     .resume(order.id, Some(Position(1))) == Right(Some(OrderResumeMarked(Some(Position(1))))))
          }
        }
      }

      "Ready" - {
        val readyOrder = suspendedOrder.copy(state = Order.Ready)

        "Detached" in {
          testEventSource(readyOrder, detached) { (order, controller, _) =>
            assert(controller.nextEvent(order.id) == None)
            assert(controller.cancel(order.id, CancelMode.FreshOnly     ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelled)))
            assert(controller.suspend(order.id) == Right(None))
            assert(controller.resume(order.id, None) == Right(Some(OrderResumed())))
            assert(controller.resume(order.id, Some(Position(1))) == Right(Some(OrderResumed(Some(Position(1))))))
          }
        }

        "Attaching" in {
          testEventSource(readyOrder, attaching) { (order, controller, agent) =>
            assert(controller.nextEvent(order.id) == None)
            assert(agent     .nextEvent(order.id) == None)
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(controller.suspend(order.id) == Right(None))
            assert(agent     .suspend(order.id) == Right(None))
            assert(controller.resume(order.id, None) == Right(Some(OrderResumeMarked())))
            assert(agent     .resume(order.id, None) == Right(Some(OrderResumeMarked())))
            assert(controller.resume(order.id, Some(Position(1))) == Right(Some(OrderResumeMarked(Some(Position(1))))))
            assert(agent     .resume(order.id, Some(Position(1))) == Right(Some(OrderResumeMarked(Some(Position(1))))))
          }
        }

        "Attached" in {
          testEventSource(readyOrder, attached) { (order, controller, agent) =>
            assert(controller.nextEvent(order.id) == None)
            assert(agent     .nextEvent(order.id) == None)
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderDetachable)))
            assert(controller.resume(order.id, None) == Right(Some(OrderResumeMarked())))
            assert(agent     .resume(order.id, None) == Right(Some(OrderResumed())))
            assert(controller.resume(order.id, Some(Position(1))) == Right(Some(OrderResumeMarked(Some(Position(1))))))
            assert(agent     .resume(order.id, Some(Position(1))) == Right(Some(OrderResumed(Some(Position(1))))))
          }
        }

        "Detaching" in {
          testEventSource(readyOrder, detaching) { (order, controller, agent) =>
            assert(controller.nextEvent(order.id) == None)
            assert(agent     .nextEvent(order.id) == None)
            assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
            assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
            assert(controller.suspend(order.id) == Right(None))
            assert(agent     .suspend(order.id) == Right(None))
            assert(controller.resume(order.id, None) == Right(Some(OrderResumeMarked())))
            assert(agent     .resume(order.id, None) == Right(Some(OrderResumeMarked())))
            assert(controller.resume(order.id, Some(Position(1))) == Right(Some(OrderResumeMarked(Some(Position(1))))))
            assert(agent     .resume(order.id, Some(Position(1))) == Right(Some(OrderResumeMarked(Some(Position(1))))))
          }
        }
      }

      "Processing Attached" in {
        val processingOrder = suspendedOrder.copy(state = Order.Processing)
        testEventSource(processingOrder, attached) { (order, controller, agent) =>
          assert(controller.nextEvent(order.id) == None)
          assert(agent     .nextEvent(order.id) == None)
          assert(controller.cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
          assert(agent     .cancel(order.id, CancelMode.FreshOnly       ) == Left(CancelStartedOrderProblem(order.id)))
          assert(controller.cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(agent     .cancel(order.id, CancelMode.FreshOrStarted()) == Right(Some(OrderCancelMarked(CancelMode.FreshOrStarted()))))
          assert(controller.suspend(order.id) == Right(None))
          assert(agent     .suspend(order.id) == Right(None))
          assert(controller.resume(order.id, None) == Right(Some(OrderResumeMarked())))
          assert(agent     .resume(order.id, None) == Right(Some(OrderResumeMarked())))
          assert(controller.resume(order.id, Some(Position(1))) == Right(Some(OrderResumeMarked(Some(Position(1))))))
          assert(agent     .resume(order.id, Some(Position(1))) == Right(Some(OrderResumeMarked(Some(Position(1))))))
        }
      }
    }

    def testEventSource(templateOrder: Order[Order.State], attachedState: Option[Order.AttachedState])
      (body: (Order[Order.State], OrderEventSource, OrderEventSource) => Unit)
    = {
      val order = templateOrder.copy(attachedState = attachedState)
      def eventSource(isAgent: Boolean) = new OrderEventSource(
        Map(TestWorkflowId -> ForkWorkflow).checked,
        Map(order.id -> order).checked,
        isAgent = isAgent)
      body(order, eventSource(isAgent = false), eventSource(isAgent = true))
    }
  }

  "Try catch" - {
    val workflow = WorkflowParser.parse(
       """define workflow {
         |  try {                                     // 0
         |    try {                                   // 0/0:0
         |      execute agent="a", executable="/ex";  // 0/0:0/0:0
         |    } catch {
         |      execute agent="a", executable="/ex";  // 0/0:0/1:0
         |    }
         |  } catch {
         |    execute agent="a", executable="/ex";    // 0/1:0
         |    try {                                   // 0/1:1
         |      execute agent="a", executable="/ex";  // 0/1:1/0:0
         |    } catch {
         |      execute agent="a", executable="/ex";  // 0/1:1/1:0
         |    }
         |  };
         |  execute agent="a", executable="/ex";      // 1
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
           |        execute agent="a", executable="/ex";   // 0/try:0/fork+🥕:0    // FAILS
           |      },
           |      "🍋": {
           |        execute agent="a", executable="/ex";   // 0/try:0/fork+🍋:0    // succeeds
           |      }
           |    }
           |  catch {
           |    execute agent="a", executable="/ex";       // 0/catch:0
           |  };
           |  execute agent="a", executable="/ex";         // 1
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
  private val TestAgentName = AgentName("AGENT")
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

  private val executeScript = Execute(WorkflowJob(AgentName("AGENT"), ExecutablePath("/executable")))

  private def step(workflow: Workflow, outcome: Outcome): Option[OrderEvent] = {
    val process = new SingleOrderProcess(workflow)
    process.update(OrderAdded(workflow.id))
    process.transferToAgent(TestAgentName)
    process.update(OrderStarted)
    process.jobStep(outcome = outcome)
    process.step()
  }

  final class SingleOrderProcess(val workflow: Workflow, val orderId: OrderId = OrderId("ORDER")) {
    private val process = new Process(workflow)

    def transferToAgent(agentName: AgentName) = {
      update(OrderAttachable(agentName))
      update(OrderAttached(agentName))
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
              Some(order.id <-: OrderAttachable(TestAgentName))
            else if (order.isAttaching)
              Some(order.id <-: OrderAttached(TestAgentName))
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
