package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.KeyedEvent
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderCoreEvent, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderTransitionedEvent}
import com.sos.jobscheduler.data.order.Outcome.Bad.AgentRestarted
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.Instruction.simplify._
import com.sos.jobscheduler.data.workflow.Instruction.{ExplicitEnd, Gap, Goto, IfErrorGoto, IfReturnCode, Job}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.TestWorkflow
import com.sos.jobscheduler.data.workflow.{AgentJobPath, JobPath, Position, Workflow, WorkflowPath}
import com.sos.jobscheduler.shared.workflow.WorkflowProcessTest._
import org.scalatest.FreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class WorkflowProcessTest extends FreeSpec {

  "AgentRestarted" in {
    assert(exec(TestWorkflow, makeOrder(Position(0), Order.Processed, Outcome.Bad(AgentRestarted))) ==
      Some(okayOrderId <-: OrderMoved(Position(0))))  // Move to same InstructionNr, and set Order.Ready
  }

  "If returnCode" - {
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
      process.update(OrderAdded(WorkflowPath("/WORKFLOW"), Order.Ready))
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

  "If returnCode else" - {
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

  "applyTransitionInstructions" - {
    "Goto, IfErrorGoto" in {
      val workflow = Workflow(Vector(
                 job,            // 0
                 Goto("B"),      // 1
                 Gap,            // 2
        "C" @:   job,            // 3
        "END" @: ExplicitEnd,    // 4
        "B" @:   IfErrorGoto("C"))) // 5
      val process = new WorkflowProcess(workflow, List(okayOrder, errorOrder))
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
      val process = new WorkflowProcess(workflow, List(okayOrder, errorOrder))
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(0)) == None)  // Loop
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(1)) == None)  // Loop
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(2)) == Some(Position(3)))  // No loop
      assert(process.applyTransitionInstructions(errorOrder.withInstructionNr(1)) == None)  // Loop
    }

    "Job, ForkJoin" in {
      val process = new WorkflowProcess(TestWorkflow, List(okayOrder, errorOrder))
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(0)) == Some(Position(0)))
      assert(process.applyTransitionInstructions(okayOrder.withInstructionNr(1)) == Some(Position(1)))
    }

    "In forked order" in {
      val process = new WorkflowProcess(TestWorkflow, List(okayOrder, errorOrder))
      val forkedOrder = okayOrder.copy(workflowPosition = okayOrder.workflowPosition.copy(position = Position(1, "🥕", 1)))
      assert(process.applyTransitionInstructions(forkedOrder) == Some(Position(1, "🥕", 1)))
    }
  }
}

object WorkflowProcessTest {
  private val okayOrderId = OrderId("OKAY")
  private val okayOrder = Order(okayOrderId, WorkflowPath("/WORKFLOW"), Order.Ready, payload = Payload(Map(), Outcome.Good(true)))
  private val errorOrder = Order(OrderId("ERROR"), WorkflowPath("/WORKFLOW"), Order.Ready, payload = Payload(Map(), Outcome.Good(false)))
  private val job = Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB")))


  private def step(workflow: Workflow, outcome: Outcome): Option[OrderTransitionedEvent] = {
    val process = new SingleOrderProcess(workflow)
    process.update(OrderAdded(WorkflowPath("/WORKFLOW"), Order.Ready))
    process.jobStep(outcome = outcome)
    process.step()
  }

  final class SingleOrderProcess(val workflow: Workflow, val orderId: OrderId = OrderId("ORDER")) {
    private val process = new Process(workflow)

    def jobStep(variablesDiff: MapDiff[String, String] = MapDiff.empty, outcome: Outcome = Outcome.Good(true)) =
      process.jobStep(orderId, variablesDiff, outcome)

    def step(): Option[OrderTransitionedEvent] =
      process.step(orderId) map (_.event)

    def update(event: OrderEvent) = process.update(orderId <-: event)
  }

  final class Process(workflow: Workflow) {
    val idToOrder = mutable.Map[OrderId, Order[Order.State]]()
    private val process = new WorkflowProcess(workflow, idToOrder)

    def jobStep(orderId: OrderId, variablesDiff: MapDiff[String, String] = MapDiff.empty, outcome: Outcome = Outcome.Good(true)): Unit = {
      update(orderId <-: OrderProcessingStarted)
      update(orderId <-: OrderProcessed(variablesDiff, outcome))
    }

    def step(orderId: OrderId): Option[KeyedEvent[OrderTransitionedEvent]] = {
      val keyedEventOption = process.tryExecuteInstruction(orderId)
      keyedEventOption foreach update
      keyedEventOption
    }

    def update(keyedEvent: KeyedEvent[OrderEvent]): Unit = {
      val KeyedEvent(orderId, event) = keyedEvent
      event match {
        case event: OrderAdded ⇒
          idToOrder.insert(orderId → Order.fromOrderAdded(orderId, event))
        case event: OrderCoreEvent ⇒
          idToOrder(orderId) = idToOrder(orderId).update(event)
        case _ ⇒
          sys.error(s"Unhandled: $event")
      }
    }
  }

  private def makeOrder(position: Position, state: Order.State, outcome: Outcome = Outcome.Good(true)) =
    okayOrder.copy(
      state = state,
      payload = Payload(Map.empty, outcome = outcome))
      .withPosition(position)

  private def exec(workflow: Workflow, order: Order[Order.State]): Option[KeyedEvent[OrderTransitionedEvent]] = {
    val process = new WorkflowProcess(workflow, Map(order.id → order))
    process.tryExecuteInstruction(order.id)
  }
}
