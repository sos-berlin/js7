package com.sos.jobscheduler.data.workflow

import cats.syntax.option.catsSyntaxOptionId
import com.sos.jobscheduler.base.circeutils.CirceUtils.JsonStringInterpolator
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.workflow.Instruction.simplify._
import com.sos.jobscheduler.data.workflow.instructions.{ExplicitEnd, ForkJoin, Goto, IfNonZeroReturnCodeGoto, IfReturnCode, ImplicitEnd, Job}
import com.sos.jobscheduler.data.workflow.test.TestSetting._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowTest extends FreeSpec {

  "labelToPosition" in {
    val workflow = Workflow(Vector(
      "A" @: Job(JobPath("/JOB"), AgentPath("/AGENT")),
      IfReturnCode(List(ReturnCode(1)),
        thenWorkflow = Workflow(Vector(
          "B" @: Job(JobPath("/JOB"), AgentPath("/AGENT"))))),
      "B" @: ExplicitEnd))
    assert(workflow.labelToPosition(Nil, Label("A")) == Some(Position(0)))
    assert(workflow.labelToPosition(Nil, Label("B")) == Some(Position(2)))
    assert(workflow.labelToPosition(Position.Parent(1, 0) :: Nil, Label("B")) == Some(Position(1, 0, 0)))
  }

  "Duplicate labels" in {
    assert(intercept[RuntimeException] {
      Workflow(Vector(
        "A" @: Job(JobPath("/JOB"), AgentPath("/AGENT")),
        "A" @: Job(JobPath("/JOB"), AgentPath("/AGENT"))))
    }
    .toString contains "Duplicate labels")
  }

  "Missing Label for Goto" in {
    intercept[RuntimeException] {
      Workflow.of(Goto(Label("A")))
    }
  }

  "Missing Label for IfNonZeroReturnCodeGoto" in {
    intercept[RuntimeException] {
      Workflow.of(IfNonZeroReturnCodeGoto(Label("A")))
    }
  }

  "jobOption" in {
    assert(ComplexTestWorkflow.jobOption(Position(0)) == Some(AJob))
    assert(ComplexTestWorkflow.jobOption(Position(1)) == None)  // IfErrorCode
    assert(ComplexTestWorkflow.jobOption(Position(2)) == None)  // ForkJoin
    assert(ComplexTestWorkflow.jobOption(Position(3)) == Some(BJob))
    assert(ComplexTestWorkflow.jobOption(Position(4)) == None)  // ImplicitEnd
    assert(ComplexTestWorkflow.jobOption(Position(999)) == None)
  }

  "workflowOption" in {
    assert(ComplexTestWorkflow.workflowOption(Position(0)) == ComplexTestWorkflow.some)
    assert(ComplexTestWorkflow.workflowOption(Position(1)) == ComplexTestWorkflow.some)
    assert(ComplexTestWorkflow.workflowOption(Position(2, "🥕", 1)) == Some(
      ComplexTestWorkflow.instruction(2).asInstanceOf[ForkJoin].workflowOption(Position.BranchId("🥕")).get))
  }

  "reduce" in {
    val job = Job(JobPath("/JOB-A"), AgentPath("/AGENT"))
    val B = Label("B")
    val C = Label("C")
    val D = Label("D")
    val END = Label("END")

    val instructions = Vector[(Instruction.Labeled, Boolean)](
      (()  @: job)              → true,
      (()  @: Goto(B))          → true,
      (C   @: job)              → true,
      (()  @: Goto(D))          → true,   // reducible?
      (()  @: IfNonZeroReturnCodeGoto(D))  → false,  // reducible
      (()  @: Goto(D))          → false,  // reducible
      (D   @: job)              → true,
      (()  @: Goto(END))        → false,  // reducible
      (END @: ExplicitEnd)      → true,
      (B   @: job)              → true,
      (()  @: Goto(C))          → true)
    val a = Workflow(instructions map (_._1))
    assert(a.reduce == Workflow(instructions collect { case (s, true) ⇒ s }))
  }

  "numberedInstruction" in {
    assert(ComplexTestWorkflow.numberedInstructions == Vector[(InstructionNr, Instruction.Labeled)](
      (InstructionNr(0), AJob),
      (InstructionNr(1), ComplexTestWorkflow.instruction(1)),
      (InstructionNr(2), ComplexTestWorkflow.instruction(2)),
      (InstructionNr(3), BJob),
      (InstructionNr(4), ImplicitEnd)))
  }

  "isDefinedAt, instruction" in {
    val addressToInstruction = List(
      Position(0) → AJob,
      Position(1) → IfReturnCode(
        ReturnCode(1) :: Nil,
        thenWorkflow = Workflow.of(AJob),
        elseWorkflow = Some(Workflow.of(BJob))),
      Position(1, 0, 0) → AJob,
      Position(1, 1, 0) → BJob,
      Position(2) → ForkJoin.of(
        "🥕" → Workflow.of(AJob, AJob),
        "🍋" → Workflow.of(BJob, BJob)),
      Position(2, "🥕", 0) → AJob,
      Position(2, "🥕", 1) → AJob,
      Position(2, "🥕", 2) → ImplicitEnd,
      Position(2, "🍋", 0) → BJob,
      Position(2, "🍋", 1) → BJob,
      Position(2, "🍋", 2) → ImplicitEnd,
      Position(3) → BJob,
      Position(4) → ImplicitEnd)

    for ((address, instruction) ← addressToInstruction) {
      assert(ComplexTestWorkflow isDefinedAt address)
      assert(ComplexTestWorkflow.instruction(address) == instruction, s" - $address")
    }
    assert(!ComplexTestWorkflow.isDefinedAt(Position(0, "🥕", 0)))
    assert(!ComplexTestWorkflow.isDefinedAt(Position(0, "🥕", 3)))
    assert(!ComplexTestWorkflow.isDefinedAt(Position(999)))
  }

  "JSON" in {
    testJson(ComplexTestWorkflow, json"""{
      "instructions": [
        { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/A" },
        {
          "TYPE": "IfReturnCode",
          "returnCodes": [ 1 ],
          "then": {
            "instructions": [
              { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/A" },
              { "TYPE": "ImplicitEnd" }
            ]
          },
          "else": {
            "instructions": [
              { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/B" },
              { "TYPE": "ImplicitEnd" }
            ]
          }
        },
        {
          "TYPE": "ForkJoin",
          "branches": [
            {
              "id": "🥕",
              "workflow": {
                "instructions": [
                  { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/A" },
                  { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/A" },
                  { "TYPE": "ImplicitEnd" }
                ]
              }
            },
            {
              "id": "🍋",
              "workflow": {
                "instructions": [
                  { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/B" },
                  { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/B" },
                  { "TYPE": "ImplicitEnd" }
                ]
              }
            }
          ]
        },
        { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/B" },
        { "TYPE": "ImplicitEnd" }
      ]
    }""")
  }
}
