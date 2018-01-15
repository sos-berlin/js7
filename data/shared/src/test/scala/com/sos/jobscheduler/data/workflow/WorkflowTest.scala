package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.Instruction.{ExplicitEnd, ForkJoin, Goto, IfError, ImplicitEnd, Job}
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec
import scala.collection.immutable.ListMap

/**
  * @author Joacim Zschimmer
  */
final class WorkflowTest extends FreeSpec {

  "labelToNumber" in {
    val workflow = Workflow(Vector(
      "A" @: Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB"))),
      "B" @: ExplicitEnd))
    assert(workflow.labelToNumber(Label(("A"))) == InstructionNr(0))
    assert(workflow.labelToNumber(Label(("B"))) == InstructionNr(1))
  }

  "Duplicate labels" in {
    assert(intercept[RuntimeException] {
      Workflow(Vector(
        "A" @: Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB"))),
        "A" @: Job(AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB")))))
    }
    .toString contains "Duplicate labels")
  }

  "Missing Label for Goto" in {
    intercept[RuntimeException] {
      Workflow.of(Goto(Label("A")))
    }
  }

  "Missing Label for IfError" in {
    intercept[RuntimeException] {
      Workflow.of(IfError(Label("A")))
    }
  }

  "reduce" in {
    val agentJobPath = AgentJobPath(AgentPath("/AGENT"), JobPath("/JOB-A"))
    val B = Label("B")
    val C = Label("C")
    val D = Label("D")
    val END = Label("END")

    val instructions = Vector[(Instruction.Labeled, Boolean)](
      (()  @: Job(agentJobPath)) → true,
      (()  @: Goto(B))           → true,
      (C   @: Job(agentJobPath)) → true,
      (()  @: Goto(D))           → true,   // reducible?
      (()  @: IfError(D))        → false,  // reducible
      (()  @: Goto(D))           → false,  // reducible
      (D   @: Job(agentJobPath)) → true,
      (()  @: Goto(END))         → false,  // reducible
      (END @: ExplicitEnd)       → true,
      (B   @: Job(agentJobPath)) → true,
      (()  @: Goto(C))           → true)
    val a = Workflow(instructions map (_._1))
    assert(a.reduce == Workflow(instructions collect { case (s, true) ⇒ s }))
  }

  "flatten" in {
    assert(ForkTestSetting.TestWorkflowScript.flatten == Vector(
      (Position(0         ), () @: Job(AAgentJobPath)),
      (Position(1         ), () @: ForkTestSetting.TestWorkflowScript.instruction(1)),
      (Position(1, "🥕", 0), () @: Job(AAgentJobPath)),
      (Position(1, "🥕", 1), () @: Job(AAgentJobPath)),
      (Position(1, "🥕", 2), () @: ImplicitEnd),
      (Position(1, "🍋", 0), () @: Job(AAgentJobPath)),
      (Position(1, "🍋", 1), () @: Job(BAgentJobPath)),
      (Position(1, "🍋", 2), () @: ImplicitEnd),
      (Position(2         ), () @: Job(AAgentJobPath)),
      (Position(3         ), () @: ForkTestSetting.TestWorkflowScript.instruction(3)),
      (Position(3, "🥕", 0), () @: Job(AAgentJobPath)),
      (Position(3, "🥕", 1), () @: Job(AAgentJobPath)),
      (Position(3, "🥕", 2), () @: ImplicitEnd),
      (Position(3, "🍋", 0), () @: Job(AAgentJobPath)),
      (Position(3, "🍋", 1), () @: Job(AAgentJobPath)),
      (Position(3, "🍋", 2), () @: ImplicitEnd),
      (Position(4         ), () @: Job(AAgentJobPath)),
      (Position(5         ), () @: ForkTestSetting.TestWorkflowScript.instruction(5)),
      (Position(5, "🥕", 0), () @: Job(AAgentJobPath)),
      (Position(5, "🥕", 1), () @: Job(AAgentJobPath)),
      (Position(5, "🥕", 2), () @: ImplicitEnd),
      (Position(5, "🍋", 0), () @: Job(BAgentJobPath)),
      (Position(5, "🍋", 1), () @: Job(BAgentJobPath)),
      (Position(5, "🍋", 2), () @: ImplicitEnd),
      (Position(6         ), () @: Job(AAgentJobPath)),
      (Position(7         ), () @: ImplicitEnd)))
  }

  "isDefinedAt, instruction" in {
    val addressToInstruction = List(
      Position(0) → Job(AAgentJobPath),
      Position(1) → ForkJoin(ListMap(
        OrderId.ChildId("🥕") → Workflow.of(Job(AAgentJobPath), Job(AAgentJobPath)),
        OrderId.ChildId("🍋") → Workflow.of(Job(AAgentJobPath), Job(BAgentJobPath)))),
      Position(1, "🥕", 0) → Job(AAgentJobPath),
      Position(1, "🥕", 1) → Job(AAgentJobPath),
      Position(1, "🥕", 2) → ImplicitEnd,
      Position(1, "🍋", 0) → Job(AAgentJobPath),
      Position(1, "🍋", 1) → Job(BAgentJobPath),
      Position(1, "🍋", 2) → ImplicitEnd,
      Position(2) → Job(AAgentJobPath),
      Position(3) → ForkJoin(ListMap(
            OrderId.ChildId("🥕") → Workflow.of(Job(AAgentJobPath), Job(AAgentJobPath)),
            OrderId.ChildId("🍋") → Workflow.of(Job(AAgentJobPath), Job(AAgentJobPath)))),
      Position(3, "🥕", 0) → Job(AAgentJobPath),
      Position(3, "🥕", 1) → Job(AAgentJobPath),
      Position(3, "🥕", 2) → ImplicitEnd,
      Position(3, "🍋", 0) → Job(AAgentJobPath),
      Position(3, "🍋", 1) → Job(AAgentJobPath),
      Position(3, "🍋", 2) → ImplicitEnd,
      Position(4) → Job(AAgentJobPath),
      Position(5) → ForkJoin(ListMap(
        OrderId.ChildId("🥕") → Workflow.of(Job(AAgentJobPath), Job(AAgentJobPath)),
        OrderId.ChildId("🍋") → Workflow.of(Job(BAgentJobPath), Job(BAgentJobPath)))),
      Position(5, "🥕", 0) → Job(AAgentJobPath),
      Position(5, "🥕", 1) → Job(AAgentJobPath),
      Position(5, "🥕", 2) → ImplicitEnd,
      Position(5, "🍋", 0) → Job(BAgentJobPath),
      Position(5, "🍋", 1) → Job(BAgentJobPath),
      Position(5, "🍋", 2) → ImplicitEnd,
      Position(6) → Job(AAgentJobPath),
      Position(7) → ImplicitEnd)

    for ((address, instruction) ← addressToInstruction) {
      assert(TestWorkflowScript isDefinedAt address)
      assert(TestWorkflowScript.instruction(address) == instruction, s" - $address")
    }
    assert(!TestWorkflowScript.isDefinedAt(Position(8)))
  //assert(!TestWorkflowScript.isDefinedAt(Position(0, "🥕")))
    assert(!TestWorkflowScript.isDefinedAt(Position(0, "🥕", 0)))
  //assert(!TestWorkflowScript.isDefinedAt(Position(0, "🥕")))
    assert(!TestWorkflowScript.isDefinedAt(Position(0, "🥕", 3)))
  }

  "JSON" in {
    testJson(ForkTestSetting.TestWorkflowScript, """{
      "source": "job /JOB on /AGENT-A;\nfork(\n  \"🥕\" { job /JOB on /AGENT-A; job /JOB on /AGENT-A; },\n  \"🍋\" { job /JOB on /AGENT-A; job /JOB on /AGENT-B; });\njob /JOB on /AGENT-A;\nfork(\n  \"🥕\" { job /JOB on /AGENT-A; job /JOB on /AGENT-A; },\n  \"🍋\" { job /JOB on /AGENT-A; job /JOB on /AGENT-A; });\njob /JOB on /AGENT-A;\nfork(\n  \"🥕\" { job /JOB on /AGENT-A; job /JOB on /AGENT-A; },\n  \"🍋\" { job /JOB on /AGENT-B; job /JOB on /AGENT-B; });\njob /JOB on /AGENT-A;",
      "instructions": [
        { "TYPE": "Job", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
        {
          "TYPE": "ForkJoin",
          "idToWorkflow": [
            {
              "id": "🥕",
              "workflow": {
                "instructions": [
                  { "TYPE": "Job", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                  { "TYPE": "Job", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                  { "TYPE": "ImplicitEnd" }
                ]
              }
            }, {
              "id": "🍋",
              "workflow": {
                "instructions": [
                  { "TYPE": "Job", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                  { "TYPE": "Job", "job": { "agentPath": "/AGENT-B", "jobPath": "/JOB" }},
                  { "TYPE": "ImplicitEnd" }
                ]
              }
            }
          ]
        },
        { "TYPE": "Job", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
        {
          "TYPE": "ForkJoin",
          "idToWorkflow": [
            {
              "id": "🥕",
              "workflow": {
                "instructions": [
                  { "TYPE": "Job", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                  { "TYPE": "Job", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                  { "TYPE": "ImplicitEnd" }
                ]
              }
            }, {
              "id": "🍋",
              "workflow": {
                "instructions": [
                  { "TYPE": "Job", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                  { "TYPE": "Job", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                  { "TYPE": "ImplicitEnd" }
                ]
              }
            }
          ]
        },
        { "TYPE": "Job", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
        {
          "TYPE": "ForkJoin",
          "idToWorkflow": [
            {
              "id": "🥕",
              "workflow": {
                "instructions": [
                  { "TYPE": "Job", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                  { "TYPE": "Job", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
                  { "TYPE": "ImplicitEnd" }
                ]
              }
            }, {
              "id": "🍋",
              "workflow": {
                "instructions": [
                  { "TYPE": "Job", "job": { "agentPath": "/AGENT-B", "jobPath": "/JOB" }},
                  { "TYPE": "Job", "job": { "agentPath": "/AGENT-B", "jobPath": "/JOB" }},
                  { "TYPE": "ImplicitEnd" }
                ]
              }
            }
          ]
        },
        { "TYPE": "Job", "job": { "agentPath": "/AGENT-A", "jobPath": "/JOB" }},
        { "TYPE": "ImplicitEnd" }
      ]
    }""")
  }
}
