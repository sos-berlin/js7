package com.sos.jobscheduler.shared.workflow

import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.Instruction.{ExplicitEnd, Gap, Goto, IfError, Job}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting.TestWorkflow
import com.sos.jobscheduler.data.workflow.{AgentJobPath, InstructionNr, JobPath, Position, Workflow, WorkflowPath}
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowProcessTest extends FreeSpec {

  private val okayOrder = Order(OrderId("OKAY"), WorkflowPath("/WORKFLOW"), Order.Ready, payload = Payload(Map(), Outcome.Good(true)))
  private val errorOrder = Order(OrderId("ERROR"), WorkflowPath("/WORKFLOW"), Order.Ready, payload = Payload(Map(), Outcome.Good(false)))
  private val job = Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB-A")))

  "applyJumpInstructions" - {
    "Goto, IfError" in {
      val workflow = Workflow(Vector(
                 job,            // 0
                 Goto("B"),      // 1
                 Gap,            // 2
        "C" @:   job,            // 3
        "END" @: ExplicitEnd,    // 4
        "B" @:   IfError("C")))  // 5
      val process = new WorkflowProcess(workflow, List(okayOrder, errorOrder).toKeyedMap(_.id))
      assert(process.applyJumpInstructions(okayOrder.moveTo(0)) == Some(InstructionNr(0)))    // Job
      assert(process.applyJumpInstructions(okayOrder.moveTo(1)) == Some(InstructionNr(6)))    // success
      assert(process.applyJumpInstructions(errorOrder.moveTo(1)) == Some(InstructionNr(3)))   // error
      assert(process.applyJumpInstructions(okayOrder.moveTo(2)) == Some(InstructionNr(2)))    // Gap
      assert(process.applyJumpInstructions(okayOrder.moveTo(3)) == Some(InstructionNr(3)))    // Job
      assert(process.applyJumpInstructions(okayOrder.moveTo(4)) == Some(InstructionNr(4)))    // ExplicitEnd
      assert(process.applyJumpInstructions(okayOrder.moveTo(5)) == Some(InstructionNr(6)))    // success
      assert(process.applyJumpInstructions(errorOrder.moveTo(5)) == Some(InstructionNr(3)))   // error
      assert(process.applyJumpInstructions(okayOrder.moveTo(6)) == Some(InstructionNr(6)))    // ImplicitEnd
      intercept[RuntimeException] {
        process.applyJumpInstructions(okayOrder moveTo 99)
      }
    }

    "Jump loops are detected" in {
      val workflow = Workflow(Vector(
        "A" @: Goto("B"),       // 0
        "B" @: Goto("A"),       // 1
        "C" @: IfError("A")))   // 2
      val process = new WorkflowProcess(workflow, List(okayOrder, errorOrder).toKeyedMap(_.id))
      assert(process.applyJumpInstructions(okayOrder.moveTo(0)) == None)  // Loop
      assert(process.applyJumpInstructions(okayOrder.moveTo(1)) == None)  // Loop
      assert(process.applyJumpInstructions(okayOrder.moveTo(2)) == Some(InstructionNr(3)))  // No loop
      assert(process.applyJumpInstructions(errorOrder.moveTo(1)) == None)  // Loop
    }

    "Job, ForkJoin" in {
      val process = new WorkflowProcess(TestWorkflow, List(okayOrder, errorOrder).toKeyedMap(_.id))
      assert(process.applyJumpInstructions(okayOrder.moveTo(0)) == Some(InstructionNr(0)))
      assert(process.applyJumpInstructions(okayOrder.moveTo(1)) == Some(InstructionNr(1)))
    }

    "In forked order" in {
      val process = new WorkflowProcess(TestWorkflow, List(okayOrder, errorOrder).toKeyedMap(_.id))
      val forkedOrder = okayOrder.copy(workflowPosition = okayOrder.workflowPosition.copy(position = Position(1, "🥕", 1)))
      assert(process.applyJumpInstructions(forkedOrder) == Some(InstructionNr(1)))
    }
  }
}
