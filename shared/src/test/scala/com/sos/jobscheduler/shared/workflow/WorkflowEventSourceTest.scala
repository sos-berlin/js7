package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{<-:, KeyedEvent}
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderAdded, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted}
import com.sos.jobscheduler.data.order.Outcome.Bad.AgentRestarted
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.Instruction.simplify._
import com.sos.jobscheduler.data.workflow.Instruction.{ExplicitEnd, Gap, Goto, IfErrorGoto, IfReturnCode, Job}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting
import com.sos.jobscheduler.data.workflow.{AgentJobPath, JobPath, Position, Workflow}
import com.sos.jobscheduler.shared.workflow.WorkflowEventSource.FollowUp
import com.sos.jobscheduler.shared.workflow.WorkflowEventSourceTest.{okayOrderId, _}
import org.scalatest.FreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class WorkflowEventSourceTest extends FreeSpec {

  "AgentRestarted" in {
    assert(nextEvent(ForkTestSetting.TestNamedWorkflow.workflow, makeOrder(Position(0), Order.Processed, Outcome.Bad(AgentRestarted))) ==
      Some(okayOrderId <-: OrderMoved(Position(0))))  // Move to same InstructionNr, and set Order.Ready
  }

  "if returnCode" - {
    val workflow = Workflow.of(
      job,                                      // 0
      IfReturnCode(List(ReturnCode(0)), Vector( // 1
        Workflow.of(job))),  // then            // 1,0,0
      job)                                      // 2

    "then branch executed" in {
      assert(step(workflow, Outcome.Good(false)) == Some(OrderMoved(Position(2))))
    }

    "again, all events" in {
      val process = new SingleOrderProcess(workflow)
      process.update(OrderAdded(TestWorkflowPath, Order.Ready))
      process.jobStep()
      assert(process.step() == Some(OrderMoved(Position(1, 0, 0))))
      process.jobStep()
      assert(process.step() == Some(OrderMoved(Position(2))))
      process.jobStep()
      assert(process.step() == Some(OrderMoved(Position(3))))
      assert(process.step() == Some(OrderFinished))
    }

    "then branch not executed" in {
      assert(step(workflow, Outcome.Good(false)) == Some(OrderMoved(Position(2))))
    }
  }

  "if returnCode else" - {
    val workflow = Workflow.of(
      job,                                      // 0
      IfReturnCode(List(ReturnCode(0)), Vector( // 1
        Workflow.of(job),    // then            // 1,0,0
        Workflow.of(job))),  // else            // 1,1,0
      job)                                      // 2

    "then branch executed" in {
      assert(step(workflow, Outcome.Good(true)) == Some(OrderMoved(Position(1, 0, 0))))
    }

    "else branch executed" in {
      assert(step(workflow, Outcome.Good(false)) == Some(OrderMoved(Position(1, 1, 0))))
    }
  }

  "fork" in {
    val workflow = ForkTestSetting.TestWorkflow
    val process = new Process(workflow)
    val orderId = okayOrderId

    process.update(orderId <-: OrderAdded(TestWorkflowPath, Order.StartNow))
    assert(process.run(orderId) == List(
      orderId <-: OrderProcessingStarted,
      orderId <-: OrderProcessed(MapDiff.empty, Outcome.Default),
      orderId <-: OrderMoved(Position(1)),
      orderId <-: OrderForked(List(
        OrderForked.Child("🥕", orderId / "🥕", MapDiff.empty),
        OrderForked.Child("🍋", orderId / "🍋", MapDiff.empty)))))

    assert(process.run(orderId / "🥕") == List(
      orderId / "🥕" <-: OrderProcessingStarted,
      orderId / "🥕" <-: OrderProcessed(MapDiff.empty, Outcome.Default),
      orderId / "🥕" <-: OrderMoved(Position(1, "🥕", 1)),
      orderId / "🥕" <-: OrderProcessingStarted,
      orderId / "🥕" <-: OrderProcessed(MapDiff.empty, Outcome.Default),
      orderId / "🥕" <-: OrderMoved(Position(1, "🥕", 2))))

    assert(process.step(orderId).isEmpty)  // Nothing to join

    assert(process.run(orderId / "🍋") == List(
      orderId / "🍋" <-: OrderProcessingStarted,
      orderId / "🍋" <-: OrderProcessed(MapDiff.empty, Outcome.Default),
      orderId / "🍋" <-: OrderMoved(Position(1, "🍋", 1)),
      orderId / "🍋" <-: OrderProcessingStarted,
      orderId / "🍋" <-: OrderProcessed(MapDiff.empty, Outcome.Default),
      orderId / "🍋" <-: OrderMoved(Position(1, "🍋", 2)),
      orderId <-: OrderJoined(Position(2), MapDiff.empty, Outcome.Default)))

    assert(process.step(orderId) == Some(orderId <-: OrderProcessingStarted))
    // and so forth...
  }

  "applyTransitionInstructions" - {
    "Goto, IfErrorGoto" in {
      val workflow = Workflow(Vector(
                 job,            // 0
                 Goto("B"),      // 1
                 Gap,            // 2
        "C" @:   job,            // 3
        "END" @: ExplicitEnd,    // 4
        "B" @:   IfErrorGoto("C"))) // 5
      val process = newWorkflowEventSource(workflow, List(okayOrder, errorOrder))
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(0)) == Some(Position(0)))    // Job
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(1)) == Some(Position(6)))    // success
      assert(process.applyTransitionInstructions(errorOrder.withInstructionNr(1)) == Some(Position(3)))   // error
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(2)) == Some(Position(2)))    // Gap
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(3)) == Some(Position(3)))    // Job
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(4)) == Some(Position(4)))    // ExplicitEnd
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(5)) == Some(Position(6)))    // success
      assert(process.applyTransitionInstructions(errorOrder.withInstructionNr(5)) == Some(Position(3)))   // error
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(6)) == Some(Position(6)))    // ImplicitEnd
      intercept[RuntimeException] {
        process.applyTransitionInstructions(okayOrder withInstructionNr 99)
      }
    }

    "Jump loops are detected" in {
      val workflow = Workflow(Vector(
        "A" @: Goto("B"),           // 0
        "B" @: Goto("A"),           // 1
        "C" @: IfErrorGoto("A")))   // 2
      val process = newWorkflowEventSource(workflow, List(okayOrder, errorOrder))
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(0)) == None)  // Loop
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(1)) == None)  // Loop
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(2)) == Some(Position(3)))  // No loop
      assert(process.applyTransitionInstructions(errorOrder.withInstructionNr(1)) == None)  // Loop
    }

    "Job, ForkJoin" in {
      val process = newWorkflowEventSource(ForkTestSetting.TestWorkflow, List(okayOrder, errorOrder))
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(0)) == Some(Position(0)))
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(1)) == Some(Position(1)))
    }

    "In forked order" in {
      val process = newWorkflowEventSource(ForkTestSetting.TestWorkflow, List(okayOrder, errorOrder))
      val forkedOrder = okayOrder.copy(workflowPosition = okayOrder.workflowPosition.copy(position = Position(1, "🥕", 1)))
      assert(process.applyTransitionInstructions(forkedOrder) == Some(Position(1, "🥕", 1)))
    }
  }
}

object WorkflowEventSourceTest {
  private val TestWorkflowPath = ForkTestSetting.TestNamedWorkflow.path
  private val okayOrderId = OrderId("OKAY")
  private val okayOrder = Order(okayOrderId, TestWorkflowPath, Order.Ready, payload = Payload(Map(), Outcome.Good(true)))
  private val errorOrder = Order(OrderId("ERROR"), TestWorkflowPath, Order.Ready, payload = Payload(Map(), Outcome.Good(false)))
  private val job = Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB")))

  private def step(workflow: Workflow, outcome: Outcome): Option[OrderEvent] = {
    val process = new SingleOrderProcess(workflow)
    process.update(OrderAdded(TestWorkflowPath, Order.Ready))
    process.jobStep(outcome = outcome)
    process.step()
  }

  final class SingleOrderProcess(val workflow: Workflow, val orderId: OrderId = OrderId("ORDER")) {
    private val process = new Process(workflow)

    def jobStep(variablesDiff: MapDiff[String, String] = MapDiff.empty, outcome: Outcome = Outcome.Good(true)) =
      process.jobStep(orderId, variablesDiff, outcome)

    def step(): Option[OrderEvent] =
      process.step(orderId) map (_.event)

    def update(event: OrderEvent) = process.update(orderId <-: event)
  }

  final class Process(workflow: Workflow) {
    val idToOrder = mutable.Map[OrderId, Order[Order.State]]()
    private val eventSource = new WorkflowEventSource(Map(TestWorkflowPath → workflow), idToOrder)
    private val eventHandler = new WorkflowEventHandler(idToOrder)
    private val inProcess = mutable.Set[OrderId]()

    def jobStep(orderId: OrderId, variablesDiff: MapDiff[String, String] = MapDiff.empty, outcome: Outcome = Outcome.Good(true)): Unit = {
      update(orderId <-: OrderProcessingStarted)
      update(orderId <-: OrderProcessed(variablesDiff, outcome))
    }

    def processed(orderId: OrderId, variablesDiff: MapDiff[String, String] = MapDiff.empty, outcome: Outcome = Outcome.Good(true)): Unit =
      update(orderId <-: OrderProcessed(variablesDiff, outcome))

    def run(orderId: OrderId): List[KeyedEvent[OrderEvent]] = {
      step(orderId) match {
        case Some(keyedEvent) ⇒ keyedEvent :: (if (idToOrder contains orderId) run(orderId) else Nil)
        case _ ⇒ Nil
      }
    }

    def step(orderId: OrderId): Option[KeyedEvent[OrderEvent]] = {
      val keyedEventOption = nextEvent(orderId)
      keyedEventOption foreach update
      keyedEventOption
    }

    private def nextEvent(orderId: OrderId): Option[KeyedEvent[OrderEvent]] = {
      val order = idToOrder(orderId)
      (order.state, workflow.instruction(order.position)) match {
        case (_: Order.Idle/*Ready!!!*/, _: Job) ⇒
          Some(order.id <-: OrderProcessingStarted)

        case _ if inProcess contains orderId ⇒
          Some(orderId <-: OrderProcessed(MapDiff.empty, Outcome.Default))

        case _ ⇒
          eventSource.nextEvent(orderId)
      }
    }

    def update(keyedEvent: KeyedEvent[OrderEvent]): Unit = {
      val KeyedEvent(orderId, event) = keyedEvent
      event match {
        case event: OrderAdded ⇒
          idToOrder.insert(orderId → Order.fromOrderAdded(orderId, event))

        case event: OrderCoreEvent ⇒
          processEvent(keyedEvent)
          idToOrder(orderId) = idToOrder(orderId).update(event)

        case _ ⇒
          sys.error(s"Unhandled: $event")
      }
    }

    private def processEvent(keyedEvent: KeyedEvent[OrderEvent]): Unit =
      keyedEvent match {
        case orderId <-: OrderProcessingStarted ⇒
          inProcess += orderId

        case orderId <-: (_: OrderProcessed) ⇒
          inProcess -= orderId

        case _ ⇒
          eventHandler.handleEvent(keyedEvent) foreach {
            case FollowUp.Add(derivedOrder) ⇒
              idToOrder.insert(derivedOrder.id → derivedOrder)

            case FollowUp.Remove(removeOrderId) ⇒
              idToOrder -= removeOrderId
          }
      }
  }

  private def makeOrder(position: Position, state: Order.State, outcome: Outcome = Outcome.Good(true)) =
    okayOrder.copy(
      state = state,
      payload = Payload(Map.empty, outcome = outcome))
      .withPosition(position)

  private def nextEvent(workflow: Workflow, order: Order[Order.State]): Option[KeyedEvent[OrderActorEvent]] = {
    val eventSource = new WorkflowEventSource(Map(TestWorkflowPath → workflow), Map(order.id → order))
    eventSource.nextEvent(order.id)
  }

  private def newWorkflowEventSource(workflow: Workflow, orders: Iterable[Order[Order.State]]) =
    new WorkflowEventSource(Map(TestWorkflowPath → workflow), orders toKeyedMap (_.id))
}
