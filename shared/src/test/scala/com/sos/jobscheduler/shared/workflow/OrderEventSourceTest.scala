package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{<-:, KeyedEvent}
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderEvent.{OrderActorEvent, OrderAdded, OrderCoreEvent, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted}
import com.sos.jobscheduler.data.order.{Order, OrderEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.Instruction.simplify._
import com.sos.jobscheduler.data.workflow.instructions.{ExplicitEnd, Gap, Goto, IfNonZeroReturnCodeGoto, IfReturnCode, Job}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting
import com.sos.jobscheduler.data.workflow.{JobPath, Position, Workflow}
import com.sos.jobscheduler.shared.workflow.OrderEventHandler.FollowUp
import com.sos.jobscheduler.shared.workflow.OrderEventSourceTest._
import org.scalatest.FreeSpec
import scala.collection.mutable

/**
  * @author Joacim Zschimmer
  */
final class OrderEventSourceTest extends FreeSpec {

  "JobSchedulerRestarted" in {
    assert(nextEvent(ForkTestSetting.TestNamedWorkflow.workflow, disruptedOrder) ==
      Some(disruptedOrder.id <-: OrderMoved(Position(0))))  // Move to same InstructionNr, and set Order.Ready
  }

  "if returnCode" - {
    val workflow = Workflow.of(
      job,                                      // 0
      IfReturnCode(List(ReturnCode(0)), Vector( // 1
        Workflow.of(job))),  // then            // 1,0,0
      job)                                      // 2

    "then branch executed" in {
      assert(step(workflow, Outcome.Succeeded(ReturnCode(1))) == Some(OrderMoved(Position(2))))
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
      assert(step(workflow, Outcome.Succeeded(ReturnCode(1))) == Some(OrderMoved(Position(2))))
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
      assert(step(workflow, Outcome.succeeded) == Some(OrderMoved(Position(1, 0, 0))))
    }

    "else branch executed" in {
      assert(step(workflow, Outcome.Succeeded(ReturnCode(1))) == Some(OrderMoved(Position(1, 1, 0))))
    }
  }

  "fork" in {
    val workflow = ForkTestSetting.TestWorkflow
    val process = new Process(workflow)
    val orderId = succeededOrderId

    process.update(orderId <-: OrderAdded(TestWorkflowPath, Order.StartNow))
    assert(process.run(orderId) == List(
      orderId <-: OrderProcessingStarted,
      orderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      orderId <-: OrderMoved(Position(1)),
      orderId <-: OrderForked(List(
        OrderForked.Child("🥕", orderId / "🥕", MapDiff.empty),
        OrderForked.Child("🍋", orderId / "🍋", MapDiff.empty)))))

    assert(process.run(orderId / "🥕") == List(
      orderId / "🥕" <-: OrderProcessingStarted,
      orderId / "🥕" <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      orderId / "🥕" <-: OrderMoved(Position(1, "🥕", 1)),
      orderId / "🥕" <-: OrderProcessingStarted,
      orderId / "🥕" <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      orderId / "🥕" <-: OrderMoved(Position(1, "🥕", 2))))

    assert(process.step(orderId).isEmpty)  // Nothing to join

    assert(process.run(orderId / "🍋") == List(
      orderId / "🍋" <-: OrderProcessingStarted,
      orderId / "🍋" <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      orderId / "🍋" <-: OrderMoved(Position(1, "🍋", 1)),
      orderId / "🍋" <-: OrderProcessingStarted,
      orderId / "🍋" <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      orderId / "🍋" <-: OrderMoved(Position(1, "🍋", 2)),
      orderId <-: OrderJoined(MapDiff.empty, Outcome.succeeded)))

    assert(process.step(orderId) == Some(orderId <-: OrderMoved(Position(2))))
    assert(process.step(orderId) == Some(orderId <-: OrderProcessingStarted))
    // and so forth...
  }

  "applyMoveInstructions" - {
    "Goto, IfFailedGoto" in {
      val workflow = Workflow(Vector(
                 job,            // 0
                 Goto("B"),      // 1
                 Gap,            // 2
        "C" @:   job,            // 3
        "END" @: ExplicitEnd,    // 4
        "B" @:   IfNonZeroReturnCodeGoto("C"))) // 5
      val eventSource = newWorkflowEventSource(workflow, List(succeededOrder, failedOrder))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(0)) == Some(Position(0)))    // Job
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1)) == Some(Position(6)))    // success
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(2)) == Some(Position(2)))    // Gap
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(3)) == Some(Position(3)))    // Job
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(4)) == Some(Position(4)))    // ExplicitEnd
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(5)) == Some(Position(6)))    // success
      assert(eventSource.applyMoveInstructions(failedOrder    withPosition Position(5)) == Some(Position(3)))    // failure
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(6)) == Some(Position(6)))    // ImplicitEnd
      intercept[RuntimeException] {
        eventSource.applyMoveInstructions(succeededOrder withInstructionNr 99)
      }
    }

    "Jump loops are detected" in {
      val workflow = Workflow(Vector(
        "A" @: Goto("B"),           // 0
        "B" @: Goto("A"),           // 1
        "C" @: IfNonZeroReturnCodeGoto("A")))   // 2
      val eventSource = newWorkflowEventSource(workflow, List(succeededOrder, failedOrder))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(0)) == None)  // Loop
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1)) == None)  // Loop
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(2)) == Some(Position(3)))  // No loop
      assert(eventSource.applyMoveInstructions(failedOrder    withPosition Position(1)) == None)  // Loop
    }

    "Job, ForkJoin" in {
      val eventSource = newWorkflowEventSource(ForkTestSetting.TestWorkflow, List(succeededOrder, failedOrder))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(0)) == Some(Position(0)))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1)) == Some(Position(1)))
    }

    "In forked order" in {
      val eventSource = newWorkflowEventSource(ForkTestSetting.TestWorkflow, List(succeededOrder, failedOrder))
      assert(eventSource.applyMoveInstructions(succeededOrder withPosition Position(1, "🥕", 1)) == Some(Position(1, "🥕", 1)))
    }
  }
}

object OrderEventSourceTest {
  private val TestWorkflowPath = ForkTestSetting.TestNamedWorkflow.path
  private val succeededOrderId = OrderId("SUCCESS")
  private val succeededOrder = Order(succeededOrderId, TestWorkflowPath, Order.Processed(Outcome.Succeeded(ReturnCode.Success)))
  private val failedOrder = Order(OrderId("FAILED"), TestWorkflowPath, Order.Processed(Outcome.Succeeded(ReturnCode.StandardFailure)))
  private val disruptedOrder = Order(OrderId("DISRUPTED"), TestWorkflowPath, Order.Processed(Outcome.Disrupted(Outcome.Disrupted.JobSchedulerRestarted)))

  private val job = Job(JobPath("/JOB"), AgentPath("/AGENT"))

  private def step(workflow: Workflow, outcome: Outcome): Option[OrderEvent] = {
    val process = new SingleOrderProcess(workflow)
    process.update(OrderAdded(TestWorkflowPath, Order.Ready))
    process.jobStep(outcome = outcome)
    process.step()
  }

  final class SingleOrderProcess(val workflow: Workflow, val orderId: OrderId = OrderId("ORDER")) {
    private val process = new Process(workflow)

    def jobStep(variablesDiff: MapDiff[String, String] = MapDiff.empty, outcome: Outcome = Outcome.Succeeded(ReturnCode.Success)) =
      process.jobStep(orderId, variablesDiff, outcome)

    def step(): Option[OrderEvent] =
      process.step(orderId) map (_.event)

    def update(event: OrderEvent) = process.update(orderId <-: event)
  }

  final class Process(workflow: Workflow) {
    val idToOrder = mutable.Map[OrderId, Order[Order.State]]()
    private val eventSource = new OrderEventSource(Map(TestWorkflowPath → workflow), idToOrder)
    private val eventHandler = new OrderEventHandler(idToOrder)
    private val inProcess = mutable.Set[OrderId]()

    def jobStep(orderId: OrderId, variablesDiff: MapDiff[String, String] = MapDiff.empty, outcome: Outcome = Outcome.succeeded): Unit = {
      update(orderId <-: OrderProcessingStarted)
      update(orderId <-: OrderProcessed(variablesDiff, outcome))
    }

    def processed(orderId: OrderId, variablesDiff: MapDiff[String, String] = MapDiff.empty, outcome: Outcome = Outcome.succeeded): Unit =
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
          Some(orderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded))

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
            case FollowUp.AddChild(derivedOrder) ⇒
              idToOrder.insert(derivedOrder.id → derivedOrder)

            case FollowUp.Remove(removeOrderId) ⇒
              idToOrder -= removeOrderId
          }
      }
  }

  private def nextEvent(workflow: Workflow, order: Order[Order.State]): Option[KeyedEvent[OrderActorEvent]] = {
    val eventSource = new OrderEventSource(Map(TestWorkflowPath → workflow), Map(order.id → order))
    eventSource.nextEvent(order.id)
  }

  private def newWorkflowEventSource(workflow: Workflow, orders: Iterable[Order[Order.State]]) =
    new OrderEventSource(Map(TestWorkflowPath → workflow), orders toKeyedMap (_.id))
}
