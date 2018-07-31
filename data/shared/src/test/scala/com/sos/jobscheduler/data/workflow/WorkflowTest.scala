package com.sos.jobscheduler.data.workflow

import cats.data.Validated.Invalid
import cats.syntax.option.catsSyntaxOptionId
import com.sos.jobscheduler.base.circeutils.CirceUtils.JsonStringInterpolator
import com.sos.jobscheduler.base.problem.{Problem, ProblemException}
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.JobPath
import com.sos.jobscheduler.data.workflow.WorkflowTest._
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression.{Equal, NumericConstant, OrderReturnCode}
import com.sos.jobscheduler.data.workflow.instructions.{ExplicitEnd, ForkJoin, Goto, If, IfNonZeroReturnCodeGoto, ImplicitEnd, Job}
import com.sos.jobscheduler.data.workflow.test.TestSetting._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

/**
  * @author Joacim Zschimmer
  */
final class WorkflowTest extends FreeSpec {

  "JSON" - {
    "Workflow without WorkflowID, when placed in configuration directory" in {
      testJson[Workflow](Workflow.of(Job(JobPath("/JOB"), AgentPath("/AGENT"))),
        json"""{
          "instructions": [
            { "TYPE": "Job", "jobPath": "/JOB", "agentPath": "/AGENT" }
          ]
        }""")
    }

    "Workflow with WorkflowId" in {
      testJson[Workflow](TestWorkflow,
        json"""{
          "id": {
            "path": "/TEST",
            "versionId": "VERSION"
          },
          "instructions": [
            { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/A" },
            {
              "TYPE": "If",
              "predicate": "returnCode == 1",
              "then": {
                "instructions": [
                  { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/A" }
                ]
              },
              "else": {
                "instructions": [
                  { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/B" }
                ]
              }
            }, {
              "TYPE": "ForkJoin",
              "branches": [
                {
                  "id": "🥕",
                  "workflow": {
                    "instructions": [
                      { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/A" },
                      { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/A" }
                    ]
                  }
                }, {
                  "id": "🍋",
                  "workflow": {
                    "instructions": [
                      { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/B" },
                      { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/B" }
                    ]
                  }
                }
              ]
            },
            { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/B" }
          ]
        }""")
    }

    "Workflow without path" in {
      testJson(Workflow.of(AJob),
        json"""{
          "instructions": [
            { "TYPE": "Job", "agentPath": "/AGENT", "jobPath": "/A" }
          ]
        }""")
    }
  }

  "labelToPosition" in {
    val workflow = Workflow.of(
      "A" @: Job(JobPath("/JOB"), AgentPath("/AGENT")),
      If(Equal(OrderReturnCode, NumericConstant(1)),
        thenWorkflow = Workflow.of(
          "B" @: Job(JobPath("/JOB"), AgentPath("/AGENT")))),
      "B" @: ExplicitEnd)
    assert(workflow.labelToPosition(Nil, Label("A")) == Some(Position(0)))
    assert(workflow.labelToPosition(Nil, Label("B")) == Some(Position(2)))
    assert(workflow.labelToPosition(Position.Parent(1, 0) :: Nil, Label("B")) == Some(Position(1, 0, 0)))
  }

  "Duplicate labels" in {
    assert(intercept[RuntimeException] {
      Workflow.of(
        "A" @: Job(JobPath("/JOB"), AgentPath("/AGENT")),
        "A" @: Job(JobPath("/JOB"), AgentPath("/AGENT")))
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

  "Anonymous Job is rejected" in {
    assert(Workflow.checked(WorkflowPath.NoId, Vector(Job(JobPath.Anonymous, TestAgentPath))) ==
      Invalid(Problem("Anonymous Job in Workflow?")))
    intercept[ProblemException] {
      Workflow.of(Job(JobPath.Anonymous, TestAgentPath))
    } .getMessage shouldEqual "Anonymous Job in Workflow?"
  }

  "Anonymous Agent is rejected" in {
    assert(Workflow.checked(WorkflowPath.NoId, Vector(Job(JobPath("/JOB"), AgentPath.Anonymous))) ==
      Invalid(Problem("Anonymous Agent in Workflow?")))
  }

  "jobOption" in {
    assert(TestWorkflow.jobOption(Position(0)) == Some(AJob))
    assert(TestWorkflow.jobOption(Position(1)) == None)  // IfErrorCode
    assert(TestWorkflow.jobOption(Position(2)) == None)  // ForkJoin
    assert(TestWorkflow.jobOption(Position(3)) == Some(BJob))
    assert(TestWorkflow.jobOption(Position(4)) == None)  // ImplicitEnd
    assert(TestWorkflow.jobOption(Position(999)) == None)
  }

  "workflowOption" in {
    assert(TestWorkflow.workflowOption(Position(0)) == TestWorkflow.some)
    assert(TestWorkflow.workflowOption(Position(1)) == TestWorkflow.some)
    assert(TestWorkflow.workflowOption(Position(2, "🥕", 1)) == Some(
      TestWorkflow.instruction(2).asInstanceOf[ForkJoin].workflowOption(Position.BranchId("🥕")).get))
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
    val id = WorkflowPath("/WORKFLOW") % "VERSION"
    val a = Workflow(id, instructions map (_._1))
    assert(a.reduce == Workflow(id, instructions collect { case (s, true) ⇒ s }))
  }

  "numberedInstruction" in {
    assert(TestWorkflow.numberedInstructions == Vector[(InstructionNr, Instruction.Labeled)](
      (InstructionNr(0), AJob),
      (InstructionNr(1), TestWorkflow.instruction(1)),
      (InstructionNr(2), TestWorkflow.instruction(2)),
      (InstructionNr(3), BJob),
      (InstructionNr(4), ImplicitEnd)))
  }

  "isDefinedAt, instruction" in {
    val addressToInstruction = List(
      Position(0) → AJob,
      Position(1) → If(Equal(OrderReturnCode, NumericConstant(1)),
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
      assert(TestWorkflow isDefinedAt address)
      assert(TestWorkflow.instruction(address) == instruction, s" - $address")
    }
    assert(!TestWorkflow.isDefinedAt(Position(0, "🥕", 0)))
    assert(!TestWorkflow.isDefinedAt(Position(0, "🥕", 3)))
    assert(!TestWorkflow.isDefinedAt(Position(999)))
  }
}

object WorkflowTest {
  val TestWorkflow = Workflow.of(
    WorkflowPath("/TEST") % "VERSION",
    AJob,
    If(Equal(OrderReturnCode, NumericConstant(1)),
      thenWorkflow = Workflow.of(AJob),
      elseWorkflow = Some(Workflow.of(BJob))),
    ForkJoin.of(
      "🥕" → Workflow.of(AJob, AJob),
      "🍋" → Workflow.of(BJob, BJob)),
    BJob)
}
