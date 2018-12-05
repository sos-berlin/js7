package com.sos.jobscheduler.data.workflow

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils.JsonStringInterpolator
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.{ExecutablePath, JobKey}
import com.sos.jobscheduler.data.workflow.WorkflowTest._
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression.{BooleanConstant, Equal, NumericConstant, OrderReturnCode}
import com.sos.jobscheduler.data.workflow.instructions.{Execute, ExplicitEnd, Fork, Goto, If, IfNonZeroReturnCodeGoto, ImplicitEnd}
import com.sos.jobscheduler.data.workflow.position._
import com.sos.jobscheduler.data.workflow.test.TestSetting._
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowTest extends FreeSpec {

  "JSON" - {
    "Workflow without WorkflowID, when placed in configuration directory" in {
      testJson[Workflow](
        Workflow(WorkflowPath.NoId,
          Vector(Execute(WorkflowJob.Name("JOB"))),
          Map(WorkflowJob.Name("JOB") → WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/EXECUTABLE")))),
        json"""{
          "instructions": [
            {
              "TYPE": "Execute.Named",
              "name": "JOB"
            }
          ],
          "jobs": {
            "JOB": {
              "agentPath": "/AGENT",
              "executablePath": "/EXECUTABLE",
              "taskLimit": 1
            }
          }
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
            { "TYPE": "Execute.Anonymous", "job": { "agentPath": "/AGENT", "executablePath": "/A.cmd", "taskLimit": 8, "defaultArguments": { "JOB_A": "A-VALUE" }}},
            {
              "TYPE": "If",
              "predicate": "returnCode == 1",
              "then": {
                "instructions": [
                  { "TYPE": "Execute.Named", "name": "A" },
                  { "TYPE": "Execute.Named", "name": "B" }
                ],
                "jobs": {
                  "B": { "agentPath": "/AGENT", "executablePath": "/B.cmd", "taskLimit": 8 , "defaultArguments": { "JOB_B1": "B1-VALUE" }}}
              },
              "else": {
                "instructions": [
                  { "TYPE": "Execute.Anonymous", "job": { "agentPath": "/AGENT", "executablePath": "/B.cmd", "taskLimit": 8, "defaultArguments": { "JOB_B": "B-VALUE" }}}
                ]
              }
            }, {
              "TYPE": "Fork",
              "branches": [
                {
                  "id": "🥕",
                  "workflow": {
                    "instructions": [
                      { "TYPE": "Execute.Anonymous", "job": { "agentPath": "/AGENT", "executablePath": "/A.cmd", "taskLimit": 8, "defaultArguments": { "JOB_A": "A-VALUE" }}},
                      { "TYPE": "Execute.Named", "name": "A" }
                    ]
                  }
                }, {
                  "id": "🍋",
                  "workflow": {
                    "instructions": [
                      { "TYPE": "Execute.Anonymous", "job": { "agentPath": "/AGENT", "executablePath": "/B.cmd", "taskLimit": 8, "defaultArguments": { "JOB_B": "B-VALUE" }}},
                      { "TYPE": "Execute.Named", "name": "B" }
                    ]
                  }
                }
              ]
            },
            { "TYPE": "Execute.Anonymous", "job": { "agentPath": "/AGENT", "executablePath": "/B.cmd", "taskLimit": 8, "defaultArguments": { "JOB_B": "B-VALUE" }}}
          ],
          "jobs": {
            "A": { "agentPath": "/AGENT", "executablePath": "/A.cmd", "taskLimit": 8, "defaultArguments": { "JOB_A": "A-VALUE" }},
            "B": { "agentPath": "/AGENT", "executablePath": "/B.cmd", "taskLimit": 8, "defaultArguments": { "JOB_B": "B-VALUE" }}
          }
        }""")
    }

    "Workflow without path" in {
      testJson(Workflow.of(AExecute),
        json"""{
          "instructions": [
            {
              "TYPE": "Execute.Anonymous",
              "job": {
                "agentPath": "/AGENT",
                "executablePath": "/A.cmd",
                "taskLimit": 8,
                "defaultArguments": {
                  "JOB_A": "A-VALUE"
                }
              }
            }
          ]
        }""")
    }
  }

  "labelToPosition" in {
    val workflow = Workflow.of(
      "A" @: Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/EXECUTABLE"))),
      If(Equal(OrderReturnCode, NumericConstant(1)),
        thenWorkflow = Workflow.of(
          "B" @: Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/EXECUTABLE"))))),
      "B" @: ExplicitEnd)
    assert(workflow.labelToPosition(Nil, Label("A")) == Some(Position(0)))
    assert(workflow.labelToPosition(Nil, Label("B")) == Some(Position(2)))
    assert(workflow.labelToPosition(Position(1) / 0, Label("B")) == Some(Position(1, 0, 0)))
  }

  "Duplicate labels" in {
    assert(intercept[RuntimeException] {
      Workflow.of(
        "A" @: Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/EXECUTABLE"))),
        "A" @: Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/EXECUTABLE"))))
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
    assert(TestWorkflow.checkedExecute(Position(0)) == Valid(AExecute))
    assert(TestWorkflow.checkedExecute(Position(1)) == Invalid(Problem("Expected 'Execute' at workflow position #1 (not: If)")))
    assert(TestWorkflow.checkedExecute(Position(2)) == Invalid(Problem("Expected 'Execute' at workflow position #2 (not: Fork)")))
    assert(TestWorkflow.checkedExecute(Position(3)) == Valid(BExecute))
    assert(TestWorkflow.checkedExecute(Position(4)) == Invalid(Problem("Expected 'Execute' at workflow position #4 (not: ImplicitEnd)")))
    assert(TestWorkflow.checkedExecute(Position(999)) == Invalid(Problem("Expected 'Execute' at workflow position #999 (not: Gap)")))
  }

  "workflowOption" in {
    assert(TestWorkflow.nestedWorkflow(Nil) == Valid(TestWorkflow))
    assert(TestWorkflow.nestedWorkflow(Position(2) / "🥕") == Valid(
      TestWorkflow.instruction(2).asInstanceOf[Fork].workflow(BranchId("🥕")).orThrow))
  }

  "reduce" in {
    val job = Execute(WorkflowJob(AgentPath("/AGENT"), ExecutablePath("/EXECUTABLE-A")))
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
      (InstructionNr(0), AExecute),
      (InstructionNr(1), TestWorkflow.instruction(1)),
      (InstructionNr(2), TestWorkflow.instruction(2)),
      (InstructionNr(3), BExecute),
      (InstructionNr(4), ImplicitEnd)))
  }

  "flattendWorkflows" in {
    assert(TestWorkflow.flattenedWorkflows == Map(
      Nil → TestWorkflow,
      (Position(1) / 0) → TestWorkflow.instruction(Position(1)).asInstanceOf[If].thenWorkflow,
      (Position(1) / 1) → TestWorkflow.instruction(Position(1)).asInstanceOf[If].elseWorkflow.get,
      (Position(2) / "🥕") → TestWorkflow.instruction(Position(2)).asInstanceOf[Fork].branches(0).workflow,
      (Position(2) / "🍋") → TestWorkflow.instruction(Position(2)).asInstanceOf[Fork].branches(1).workflow,
    ))
  }

  "flattenedInstruction" in {
    assert(TestWorkflow.flattenedInstructions == Vector[(Position, Instruction.Labeled)](
      (Position(0), AExecute),
      (Position(1), TestWorkflow.instruction(1)),
      (Position(1, 0, 0), TestWorkflow.instruction(1).asInstanceOf[If].thenWorkflow.instructions(0)),
      (Position(1, 0, 1), TestWorkflow.instruction(1).asInstanceOf[If].thenWorkflow.instructions(1)),
      (Position(1, 0, 2), ImplicitEnd),
      (Position(1, 1, 0), TestWorkflow.instruction(1).asInstanceOf[If].elseWorkflow.get.instructions(0)),
      (Position(1, 1, 1), ImplicitEnd),
      (Position(2), TestWorkflow.instruction(2)),
      (Position(2, "🥕", 0), TestWorkflow.instruction(2).asInstanceOf[Fork].branches(0).workflow.instructions(0)),
      (Position(2, "🥕", 1), TestWorkflow.instruction(2).asInstanceOf[Fork].branches(0).workflow.instructions(1)),
      (Position(2, "🥕", 2), ImplicitEnd),
      (Position(2, "🍋", 0), TestWorkflow.instruction(2).asInstanceOf[Fork].branches(1).workflow.instructions(0)),
      (Position(2, "🍋", 1), TestWorkflow.instruction(2).asInstanceOf[Fork].branches(1).workflow.instructions(1)),
      (Position(2, "🍋", 2), ImplicitEnd),
      (Position(3), BExecute),
      (Position(4), ImplicitEnd)))
  }

  "completelyChecked in {" - {
    val wrongWorkflow = Workflow(
      WorkflowPath.NoId,
      Vector(
        If(BooleanConstant(true),
          Workflow.of(
            Execute.Named(AJobName/*undefined*/)))))
    "Unknown job" in {
      assert(wrongWorkflow.completelyChecked == Invalid(Problem("Unknown job 'A'")))
    }

    "Known job" in {
      val workflow = wrongWorkflow.copy(nameToJob = Map(AJobName → AJob))
      assert(workflow.completelyChecked == Valid(workflow))
    }
  }

  "namedJobs" in {
    assert(TestWorkflow.nameToJob == Map(
      AJobName → AJob,
      BJobName → BJob))
  }

  "keyToJob" in {
    assert(TestWorkflow.keyToJob == Map(
      JobKey(TestWorkflow.id /: Position(0)) → AExecute.job,
      JobKey(WorkflowBranchPath(TestWorkflow.id, Position(1) / 0), BJobName) → B1Job,
      JobKey(TestWorkflow.id /: (Position(1) / 1 / 0)) → BExecute.job,
      JobKey(TestWorkflow.id /: (Position(2) /  "🥕" / 0)) → AExecute.job,
      JobKey(TestWorkflow.id /: (Position(2) /  "🍋" / 0)) → BExecute.job,
      JobKey(TestWorkflow.id /: Position(3)) → BExecute.job,
      JobKey(TestWorkflow.id, AJobName) → AJob,
      JobKey(TestWorkflow.id, BJobName) → BJob))
  }

  "isDefinedAt, instruction" in {
    val addressToInstruction = List(
      Position(0) → AExecute,
      Position(1) → TestWorkflow.instruction(1),
      Position(1, 0, 0) → Execute.Named(AJobName),
      Position(1, 0, 1) → Execute.Named(BJobName),
      Position(1, 1, 0) → BExecute,
      Position(2) → TestWorkflow.instruction(2),
      Position(2, "🥕", 0) → AExecute,
      Position(2, "🥕", 1) → Execute.Named(AJobName),
      Position(2, "🥕", 2) → ImplicitEnd,
      Position(2, "🍋", 0) → BExecute,
      Position(2, "🍋", 1) → Execute.Named(BJobName),
      Position(2, "🍋", 2) → ImplicitEnd,
      Position(3) → BExecute,
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

private object WorkflowTest
{
  private val TestWorkflow = Workflow(
    WorkflowPath("/TEST") % "VERSION",
    Vector(
      AExecute,
      If(Equal(OrderReturnCode, NumericConstant(1)),
        thenWorkflow = Workflow.anonymous(
          Vector(
            Execute.Named(AJobName),
            Execute.Named(BJobName)),
          Map(
            BJobName → B1Job)),
        elseWorkflow = Some(Workflow.of(
          BExecute))),
      Fork.of(
        "🥕" → Workflow.of(
          AExecute,
          Execute.Named(AJobName)),
        "🍋" → Workflow.of(
          BExecute,
          Execute.Named(BJobName))),
      BExecute),
    Map(
      AJobName → AJob,
      BJobName → BJob)
  )
}
