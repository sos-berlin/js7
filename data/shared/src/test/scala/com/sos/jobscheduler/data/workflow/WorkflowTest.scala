package com.sos.jobscheduler.data.workflow

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils.JsonStringInterpolator
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.{ExecutablePath, JobKey}
import com.sos.jobscheduler.data.workflow.WorkflowTest._
import com.sos.jobscheduler.data.workflow.instructions.If.{Else, Then}
import com.sos.jobscheduler.data.workflow.instructions.TryInstruction.{Catch_, Try_, try_}
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.expr.Expression.{BooleanConstant, Equal, NumericConstant, OrderReturnCode}
import com.sos.jobscheduler.data.workflow.instructions.{Execute, ExplicitEnd, Fail, Fork, Goto, If, IfNonZeroReturnCodeGoto, ImplicitEnd, Retry, TryInstruction}
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
          Map(WorkflowJob.Name("JOB") -> WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/EXECUTABLE")))),
        json"""{
          "instructions": [
            {
              "TYPE": "Execute.Named",
              "name": "JOB"
            }
          ],
          "jobs": {
            "JOB": {
              "agentRefPath": "/AGENT",
              "executablePath": "/EXECUTABLE",
              "taskLimit": 1
            }
          }
        }""")
    }

    "Workflow with WorkflowId" in {
      testJson[Workflow](TestWorkflow,
        json"""{
          "path": "/TEST",
          "versionId": "VERSION",
          "instructions": [
            { "TYPE": "Execute.Anonymous", "job": { "agentRefPath": "/AGENT", "executablePath": "/A.cmd", "taskLimit": 3, "defaultArguments": { "JOB_A": "A-VALUE" }}},
            {
              "TYPE": "If",
              "predicate": "returnCode == 1",
              "then": {
                "instructions": [
                  { "TYPE": "Execute.Named", "name": "A" },
                  { "TYPE": "Execute.Named", "name": "B" }
                ],
                "jobs": {
                  "B": { "agentRefPath": "/AGENT", "executablePath": "/B.cmd", "taskLimit": 3 , "defaultArguments": { "JOB_B1": "B1-VALUE" }}}
              },
              "else": {
                "instructions": [
                  { "TYPE": "Execute.Anonymous", "job": { "agentRefPath": "/AGENT", "executablePath": "/B.cmd", "taskLimit": 3, "defaultArguments": { "JOB_B": "B-VALUE" }}}
                ]
              }
            }, {
              "TYPE": "Fork",
              "branches": [
                {
                  "id": "🥕",
                  "workflow": {
                    "instructions": [
                      { "TYPE": "Execute.Anonymous", "job": { "agentRefPath": "/AGENT", "executablePath": "/A.cmd", "taskLimit": 3, "defaultArguments": { "JOB_A": "A-VALUE" }}},
                      { "TYPE": "Execute.Named", "name": "A" }
                    ]
                  }
                }, {
                  "id": "🍋",
                  "workflow": {
                    "instructions": [
                      { "TYPE": "Execute.Anonymous", "job": { "agentRefPath": "/AGENT", "executablePath": "/B.cmd", "taskLimit": 3, "defaultArguments": { "JOB_B": "B-VALUE" }}},
                      { "TYPE": "Execute.Named", "name": "B" }
                    ]
                  }
                }
              ]
            },
            { "TYPE": "Execute.Anonymous", "job": { "agentRefPath": "/AGENT", "executablePath": "/B.cmd", "taskLimit": 3, "defaultArguments": { "JOB_B": "B-VALUE" }}}
          ],
          "jobs": {
            "A": { "agentRefPath": "/AGENT", "executablePath": "/A.cmd", "taskLimit": 3, "defaultArguments": { "JOB_A": "A-VALUE" }},
            "B": { "agentRefPath": "/AGENT", "executablePath": "/B.cmd", "taskLimit": 3, "defaultArguments": { "JOB_B": "B-VALUE" }}
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
                "agentRefPath": "/AGENT",
                "executablePath": "/A.cmd",
                "taskLimit": 3,
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
      "A" @: Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/EXECUTABLE"))),
      If(Equal(OrderReturnCode, NumericConstant(1)),
        thenWorkflow = Workflow.of(
          "B" @: Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/EXECUTABLE"))))),
      "B" @: ExplicitEnd)
    assert(workflow.labelToPosition(Nil, Label("A")) == Some(Position(0)))
    assert(workflow.labelToPosition(Nil, Label("B")) == Some(Position(2)))
    assert(workflow.labelToPosition(Position(1) / Then, Label("B")) == Some(Position(1) / Then % 0))
  }

  "Duplicate labels" in {
    assert(intercept[RuntimeException] {
      Workflow.of(
        "A" @: Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/EXECUTABLE"))),
        "A" @: Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/EXECUTABLE"))))
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
    assert(TestWorkflow.checkedExecute(Position(1)) == Invalid(Problem("Expected 'Execute' at workflow position :1 (not: If)")))
    assert(TestWorkflow.checkedExecute(Position(2)) == Invalid(Problem("Expected 'Execute' at workflow position :2 (not: Fork)")))
    assert(TestWorkflow.checkedExecute(Position(3)) == Valid(BExecute))
    assert(TestWorkflow.checkedExecute(Position(4)) == Invalid(Problem("Expected 'Execute' at workflow position :4 (not: ImplicitEnd)")))
    assert(TestWorkflow.checkedExecute(Position(999)) == Invalid(Problem("Expected 'Execute' at workflow position :999 (not: Gap)")))
  }

  "workflowOption" in {
    assert(TestWorkflow.nestedWorkflow(Nil) == Valid(TestWorkflow))
    assert(TestWorkflow.nestedWorkflow(Position(2) / "🥕") == Valid(
      TestWorkflow.instruction(2).asInstanceOf[Fork].workflow(BranchId("🥕")).orThrow))
  }

  "reduce" in {
    val job = Execute(WorkflowJob(AgentRefPath("/AGENT"), ExecutablePath("/EXECUTABLE-A")))
    val B = Label("B")
    val C = Label("C")
    val D = Label("D")
    val END = Label("END")

    val instructions = Vector[(Instruction.Labeled, Boolean)](
      (()  @: job)              -> true,
      (()  @: Goto(B))          -> true,
      (C   @: job)              -> true,
      (()  @: Goto(D))          -> true,   // reducible?
      (()  @: IfNonZeroReturnCodeGoto(D))  -> false,  // reducible
      (()  @: Goto(D))          -> false,  // reducible
      (D   @: job)              -> true,
      (()  @: Goto(END))        -> false,  // reducible
      (END @: ExplicitEnd)      -> true,
      (B   @: job)              -> true,
      (()  @: Goto(C))          -> true)
    val id = WorkflowPath("/WORKFLOW") % "VERSION"
    val a = Workflow(id, instructions map (_._1))
    assert(a.reduce == Workflow(id, instructions collect { case (s, true) => s }))
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
      Nil -> TestWorkflow,
      (Position(1) / Then) -> TestWorkflow.instruction(Position(1)).asInstanceOf[If].thenWorkflow,
      (Position(1) / Else) -> TestWorkflow.instruction(Position(1)).asInstanceOf[If].elseWorkflow.get,
      (Position(2) / "🥕") -> TestWorkflow.instruction(Position(2)).asInstanceOf[Fork].branches(0).workflow,
      (Position(2) / "🍋") -> TestWorkflow.instruction(Position(2)).asInstanceOf[Fork].branches(1).workflow,
    ))
  }

  "flattenedInstruction" in {
    assert(TestWorkflow.flattenedInstructions == Vector[(Position, Instruction.Labeled)](
      (Position(0), AExecute),
      (Position(1), TestWorkflow.instruction(1)),
      (Position(1) / Then % 0, TestWorkflow.instruction(1).asInstanceOf[If].thenWorkflow.instructions(0)),
      (Position(1) / Then % 1, TestWorkflow.instruction(1).asInstanceOf[If].thenWorkflow.instructions(1)),
      (Position(1) / Then % 2, ImplicitEnd),
      (Position(1) / Else % 0, TestWorkflow.instruction(1).asInstanceOf[If].elseWorkflow.get.instructions(0)),
      (Position(1) / Else % 1, ImplicitEnd),
      (Position(2), TestWorkflow.instruction(2)),
      (Position(2) / "🥕" % 0, TestWorkflow.instruction(2).asInstanceOf[Fork].branches(0).workflow.instructions(0)),
      (Position(2) / "🥕" % 1, TestWorkflow.instruction(2).asInstanceOf[Fork].branches(0).workflow.instructions(1)),
      (Position(2) / "🥕" % 2, ImplicitEnd),
      (Position(2) / "🍋" % 0, TestWorkflow.instruction(2).asInstanceOf[Fork].branches(1).workflow.instructions(0)),
      (Position(2) / "🍋" % 1, TestWorkflow.instruction(2).asInstanceOf[Fork].branches(1).workflow.instructions(1)),
      (Position(2) / "🍋" % 2, ImplicitEnd),
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
      val workflow = wrongWorkflow.copy(nameToJob = Map(AJobName -> AJob))
      assert(workflow.completelyChecked == Valid(workflow))
    }

    "retry is not allowed outside a catch block" - {
      "simple case" in {
        assert(Workflow.of(Retry()).completelyChecked == Invalid(Problem("Statement 'retry' is possible only in a catch block")))
      }
    }

    "in try" in {
      assert(Workflow.of(TryInstruction(Workflow.of(Retry()), Workflow.empty)).completelyChecked
        == Invalid(Problem("Statement 'retry' is possible only in a catch block")))
    }

    "in catch" in {
      assert(Workflow.of(TryInstruction(Workflow.empty, Workflow.of(Retry()))).completelyChecked.isValid)
    }

    "'if' in catch" in {
      assert(Workflow.of(TryInstruction(Workflow.empty, Workflow.of(If(BooleanConstant(true), Workflow.of(Retry())))))
        .completelyChecked.isValid)
    }

    "'fork' is a barrier" in {
      assert(Workflow.of(TryInstruction(Workflow.empty, Workflow.of(Fork(Vector(Fork.Branch("A", Workflow.of(Retry())))))))
        .completelyChecked == Invalid(Problem("Statement 'retry' is possible only in a catch block")))
    }
  }

  "namedJobs" in {
    assert(TestWorkflow.nameToJob == Map(
      AJobName -> AJob,
      BJobName -> BJob))
  }

  "keyToJob" in {
    assert(TestWorkflow.keyToJob == Map(
      JobKey(TestWorkflow.id /: Position(0)) -> AExecute.job,
      JobKey(WorkflowBranchPath(TestWorkflow.id, Position(1) / Then), BJobName) -> B1Job,
      JobKey(TestWorkflow.id /: (Position(1) / Else % 0)) -> BExecute.job,
      JobKey(TestWorkflow.id /: (Position(2) /  "🥕" % 0)) -> AExecute.job,
      JobKey(TestWorkflow.id /: (Position(2) /  "🍋" % 0)) -> BExecute.job,
      JobKey(TestWorkflow.id /: Position(3)) -> BExecute.job,
      JobKey(TestWorkflow.id, AJobName) -> AJob,
      JobKey(TestWorkflow.id, BJobName) -> BJob))
  }

  "anonymousJobKey" in {
    val w = Workflow(
      WorkflowPath("/TEST") % "VERSION",
      Vector(
        If(BooleanConstant(true),   // :0
          Workflow.of(Fail),        // :0/then:0
          Some(Workflow.of(Fail))), // :0/else:0
        TryInstruction(             // :1
          Workflow.of(Fail),        // :1/Try:0
          Workflow.of(Fail))))      // :1/1:0
    assert(w.anonymousJobKey(w.id /: Position(99)) == Valid(JobKey.Anonymous(w.id /: Position(99))))
    assert(w.anonymousJobKey(w.id /: (Position(0) / Then % 0)) == Valid(JobKey.Anonymous(w.id /: (Position(0) / Then % 0))))
    assert(w.anonymousJobKey(w.id /: (Position(0) / Else % 0)) == Valid(JobKey.Anonymous(w.id /: (Position(0) / Else % 0))))
    assert(w.anonymousJobKey(w.id /: (Position(1) / Try_ % 0)) == Valid(JobKey.Anonymous(w.id /: (Position(1) / Try_ % 0))))
    // anonymousJobKey normalizes the retry-index of a Retry Position to 0.
    assert(w.anonymousJobKey(w.id /: (Position(1) / try_(1)    % 0)) == Valid(JobKey.Anonymous(w.id /: (Position(1) / Try_ % 0))))
  }

  "isDefinedAt, instruction" in {
    val addressToInstruction = List(
      Position(0) -> AExecute,
      Position(1) -> TestWorkflow.instruction(1),
      Position(1) / Then % 0 -> Execute.Named(AJobName),
      Position(1) / Then % 1 -> Execute.Named(BJobName),
      Position(1) / Else % 0 -> BExecute,
      Position(2) -> TestWorkflow.instruction(2),
      Position(2) / "🥕" % 0 -> AExecute,
      Position(2) / "🥕" % 1 -> Execute.Named(AJobName),
      Position(2) / "🥕" % 2 -> ImplicitEnd,
      Position(2) / "🍋" % 0 -> BExecute,
      Position(2) / "🍋" % 1 -> Execute.Named(BJobName),
      Position(2) / "🍋" % 2 -> ImplicitEnd,
      Position(3) -> BExecute,
      Position(4) -> ImplicitEnd)

    for ((address, instruction) <- addressToInstruction) {
      assert(TestWorkflow isDefinedAt address)
      assert(TestWorkflow.instruction(address) == instruction, s" - $address")
    }
    assert(!TestWorkflow.isDefinedAt(Position(0) / "🥕" % 0))
    assert(!TestWorkflow.isDefinedAt(Position(0) / "🥕" % 3))
    assert(!TestWorkflow.isDefinedAt(Position(999)))
  }

  "findCatchPosition" in {
    assert(TestWorkflow.findCatchPosition(Position(1) / Catch_ % 0).isEmpty)

    val tryWorkflow = Workflow(
      WorkflowPath("/TEST") % "VERSION",
      Vector(
        TryInstruction(                               // :0
          tryWorkflow = Workflow.of(AExecute),        // :0/0:0
          catchWorkflow = Workflow.of(BExecute)),     // :0/1:0       catch0
        TryInstruction(
          tryWorkflow = Workflow.of(
            TryInstruction(                           // :1/0:0
              tryWorkflow = Workflow.of(AExecute),    // :1/0:0/0:0
              catchWorkflow = Workflow.of(BExecute))),// :1/0:0/1:0   catch10
          catchWorkflow = Workflow.of(                //              catch1
            TryInstruction(
              tryWorkflow = Workflow.of(AExecute),    // :1/1:0/0:0
              catchWorkflow = Workflow.of(BExecute))  // :1/1:0/1:0   catch11
          ))))
    val catch0  = Position(0) / Catch_ % 0
    val catch1  = Position(1) / Catch_ % 0
    val catch10 = Position(1) / Try_   % 0 / Catch_ % 0
    val catch11 = Position(1) / Catch_ % 0 / Catch_ % 0

    assert(tryWorkflow.findCatchPosition(Position(0)           ) == None)
    assert(tryWorkflow.findCatchPosition(Position(0) / Try_ % 0) == Some(catch0))
    assert(tryWorkflow.findCatchPosition(Position(0) / Try_ % 1) == Some(catch0))
    assert(tryWorkflow.findCatchPosition(catch0                ) == None)
    assert(tryWorkflow.findCatchPosition(catch0.increment      ) == None)

    assert(tryWorkflow.findCatchPosition(Position(1)                      ) == None)
    assert(tryWorkflow.findCatchPosition(Position(1) / Try_ % 0           ) == Some(catch1))
    assert(tryWorkflow.findCatchPosition(Position(1) / Try_ % 0 / Try_ % 0) == Some(catch10))
    assert(tryWorkflow.findCatchPosition(Position(1) / Try_ % 0 / Try_ % 1) == Some(catch10))
    assert(tryWorkflow.findCatchPosition(catch10                          ) == Some(catch1))
    assert(tryWorkflow.findCatchPosition(catch10.increment                ) == Some(catch1))
    assert(tryWorkflow.findCatchPosition(Position(1) / Try_ % 1           ) == Some(catch1))

    assert(tryWorkflow.findCatchPosition(Position(1) / Catch_ % 0 / Try_ % 0) == Some(catch11))
    assert(tryWorkflow.findCatchPosition(Position(1) / Catch_ % 0 / Try_ % 1) == Some(catch11))
    assert(tryWorkflow.findCatchPosition(catch11          ) == None)
    assert(tryWorkflow.findCatchPosition(catch11.increment) == None)
    assert(tryWorkflow.findCatchPosition(catch1           ) == None)
    assert(tryWorkflow.findCatchPosition(catch1.increment ) == None)

    assert(tryWorkflow.findCatchPosition(Position(2)) == None)
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
            BJobName -> B1Job)),
        elseWorkflow = Some(Workflow.of(
          BExecute))),
      Fork.of(
        "🥕" -> Workflow.of(
          AExecute,
          Execute.Named(AJobName)),
        "🍋" -> Workflow.of(
          BExecute,
          Execute.Named(BJobName))),
      BExecute),
    Map(
      AJobName -> AJob,
      BJobName -> BJob)
  )
}
