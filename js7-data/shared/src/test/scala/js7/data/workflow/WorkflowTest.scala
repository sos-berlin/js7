package js7.data.workflow

import io.circe.syntax._
import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.problem.Problems.UnknownKeyProblem
import js7.data.agent.AgentName
import js7.data.item.VersionId
import js7.data.job.{ExecutablePath, ExecutableScript, JobKey}
import js7.data.value.expression.Expression.{BooleanConstant, Equal, LastReturnCode, NumericConstant}
import js7.data.value.expression.PositionSearch
import js7.data.workflow.Instruction.Labeled
import js7.data.workflow.WorkflowTest._
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ExplicitEnd, Fail, Fork, Goto, If, IfFailedGoto, ImplicitEnd, Retry, TryInstruction}
import js7.data.workflow.position.BranchId.{Catch_, Else, Then, Try_, catch_, fork, try_}
import js7.data.workflow.position._
import js7.data.workflow.test.TestSetting._
import js7.tester.CirceJsonTester.{normalizeJson, removeJNull, testJson}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowTest extends AnyFreeSpec
{
  "JSON" - {
    "Workflow without WorkflowID, when placed in configuration directory" in {
      testJson[Workflow](
        Workflow(WorkflowPath.NoId,
          Vector(Labeled(Some("TEST-LABEL"), Execute(WorkflowJob.Name("JOB")))),
          Map(WorkflowJob.Name("JOB") -> WorkflowJob(AgentName("AGENT"), ExecutablePath("/EXECUTABLE")))),
        json"""{
          "instructions": [
            {
              "label": "TEST-LABEL",
              "TYPE": "Execute.Named",
              "jobName": "JOB"
            }
          ],
          "jobs": {
            "JOB": {
              "agentName": "AGENT",
              "executable": {
                "TYPE": "ExecutablePath",
                "path": "/EXECUTABLE"
              },
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
            {
              "TYPE": "Execute.Anonymous",
              "job": {
                "agentName": "AGENT",
                "executable": {
                  "TYPE": "ExecutablePath",
                  "path": "/A.cmd"
                },
                "taskLimit": 3,
                "defaultArguments": { "JOB_A": "A-VALUE" }
              }
            }, {
              "label": "TEST-LABEL",
              "TYPE": "If",
              "predicate": "$$returnCode == 1",
              "then": {
                "instructions": [
                  { "TYPE": "Execute.Named", "jobName": "A" },
                  { "TYPE": "Execute.Named", "jobName": "B" }
                ],
                "jobs": {
                  "B": {
                    "agentName": "AGENT",
                    "executable": {
                      "TYPE": "ExecutablePath",
                      "path": "/B.cmd"
                    },
                    "taskLimit": 3 ,
                    "defaultArguments": { "JOB_B1": "B1-VALUE" }
                  }
                }
              },
              "else": {
                "instructions": [
                  {
                    "TYPE": "Execute.Anonymous",
                    "job": {
                      "agentName": "AGENT",
                      "executable": {
                        "TYPE": "ExecutablePath",
                        "path": "/B.cmd"
                      },
                      "taskLimit": 3,
                      "defaultArguments": { "JOB_B": "B-VALUE" }
                    }
                  }
                ]
              }
            }, {
              "TYPE": "Fork",
              "branches": [
                {
                  "id": "🥕",
                  "workflow": {
                    "instructions": [
                      {
                        "TYPE": "Execute.Anonymous",
                        "job": {
                          "agentName": "AGENT",
                          "executable": {
                            "TYPE": "ExecutablePath",
                            "path": "/A.cmd"
                          },
                          "taskLimit": 3,
                          "defaultArguments": { "JOB_A": "A-VALUE" }
                        }
                      }, {
                        "TYPE": "Execute.Named", "jobName": "A"
                      }
                    ]
                  }
                }, {
                  "id": "🍋",
                  "workflow": {
                    "instructions": [
                      {
                        "TYPE": "Execute.Anonymous",
                        "job": {
                          "agentName": "AGENT",
                          "executable": {
                            "TYPE": "ExecutablePath",
                            "path": "/B.cmd"
                          },
                          "taskLimit": 3,
                          "defaultArguments": { "JOB_B": "B-VALUE" }
                        }
                      },
                      { "TYPE": "Execute.Named", "jobName": "B" }
                    ]
                  }
                }
              ]
            },
            {
              "TYPE": "Execute.Anonymous",
              "job": {
                "agentName": "AGENT",
                "executable": {
                  "TYPE": "ExecutablePath",
                  "path": "/B.cmd"
                },
                "taskLimit": 3,
                "defaultArguments": { "JOB_B": "B-VALUE" }
              }
            }
          ],
          "jobs": {
            "A": {
              "agentName": "AGENT",
              "executable": {
                "TYPE": "ExecutablePath",
                "path": "/A.cmd"
              },
              "taskLimit": 3,
              "defaultArguments": { "JOB_A": "A-VALUE" }
            },
            "B": {
              "agentName": "AGENT",
              "executable": {
                "TYPE": "ExecutablePath",
                "path": "/B.cmd"
              },
              "taskLimit": 3,
              "defaultArguments": { "JOB_B": "B-VALUE" }}
          }
        }""")
    }

    "Workflow with WorkflowId and positions (for JOC GUI)" in {
      assert(normalizeJson(removeJNull(TestWorkflow.withPositions(Nil).asJson)) ==
        normalizeJson(json"""{
          "path": "/TEST",
          "versionId": "VERSION",
          "instructions": [
            {
              "position": [ 0 ],
              "TYPE": "Execute.Anonymous",
              "job": {
                "agentName": "AGENT",
                "executable": {
                  "TYPE": "ExecutablePath",
                  "path": "/A.cmd"
                },
                "taskLimit": 3,
                "defaultArguments": { "JOB_A": "A-VALUE" }
              }
            }, {
              "position": [ 1 ],
              "label": "TEST-LABEL",
              "TYPE": "If",
              "predicate": "$$returnCode == 1",
              "then": {
                "instructions": [
                  { "position": [ 1, "then", 0 ], "TYPE": "Execute.Named", "jobName": "A" },
                  { "position": [ 1, "then", 1 ], "TYPE": "Execute.Named", "jobName": "B" },
                  { "position": [ 1, "then", 2 ], "TYPE": "ImplicitEnd" }
                ],
                "jobs": {
                  "B": {
                    "agentName": "AGENT",
                    "executable": {
                      "TYPE": "ExecutablePath",
                      "path": "/B.cmd"
                    },
                    "taskLimit": 3 ,
                    "defaultArguments": { "JOB_B1": "B1-VALUE" }
                  }
                }
              },
              "else": {
                "instructions": [
                  {
                    "position": [ 1, "else", 0 ],
                    "TYPE": "Execute.Anonymous",
                    "job": {
                      "agentName": "AGENT",
                      "executable": {
                        "TYPE": "ExecutablePath",
                        "path": "/B.cmd"
                      },
                      "taskLimit": 3,
                      "defaultArguments": { "JOB_B": "B-VALUE" }
                    }
                  }, {
                  "position": [ 1, "else", 1 ],
                  "TYPE": "ImplicitEnd"
                  }
                ]
              }
            }, {
              "position": [ 2 ],
              "TYPE": "Fork",
              "branches": [
                {
                  "id": "🥕",
                  "workflow": {
                    "instructions": [
                      {
                        "position": [ 2, "fork+🥕", 0 ],
                        "TYPE": "Execute.Anonymous",
                        "job": {
                          "agentName": "AGENT",
                          "executable": {
                            "TYPE": "ExecutablePath",
                            "path": "/A.cmd"
                          },
                          "taskLimit": 3,
                          "defaultArguments": { "JOB_A": "A-VALUE" }
                        }
                      }, {
                        "position": [ 2, "fork+🥕", 1 ],
                        "TYPE": "Execute.Named",
                        "jobName": "A"
                      }, {
                        "position": [ 2, "fork+🥕", 2 ],
                        "TYPE": "ImplicitEnd"
                      }
                    ]
                  }
                }, {
                  "id": "🍋",
                  "workflow": {
                    "instructions": [
                      {
                        "position": [ 2, "fork+🍋", 0 ],
                        "TYPE": "Execute.Anonymous",
                        "job": {
                          "agentName": "AGENT",
                          "executable": {
                            "TYPE": "ExecutablePath",
                            "path": "/B.cmd"
                          },
                          "taskLimit": 3,
                          "defaultArguments": { "JOB_B": "B-VALUE" }
                        }
                      }, {
                        "position": [ 2, "fork+🍋", 1 ],
                        "TYPE": "Execute.Named",
                        "jobName": "B"
                      }, {
                        "position": [ 2, "fork+🍋", 2 ],
                        "TYPE": "ImplicitEnd"
                      }
                    ]
                  }
                }
              ]
            }, {
              "position": [ 3 ],
              "TYPE": "Execute.Anonymous",
              "job": {
                "agentName": "AGENT",
                "executable": {
                  "TYPE": "ExecutablePath",
                  "path": "/B.cmd"
                },
                "taskLimit": 3,
                "defaultArguments": { "JOB_B": "B-VALUE" }
              }
            }, {
              "position": [ 4 ],
              "TYPE": "ImplicitEnd"
            }
          ],
          "jobs": {
            "A": {
              "agentName": "AGENT",
              "executable": {
                "TYPE": "ExecutablePath",
                "path": "/A.cmd"
              },
              "taskLimit": 3,
              "defaultArguments": { "JOB_A": "A-VALUE" }
            },
            "B": {
              "agentName": "AGENT",
              "executable": {
                "TYPE": "ExecutablePath",
                "path": "/B.cmd"
              },
              "taskLimit": 3,
              "defaultArguments": { "JOB_B": "B-VALUE" }}
          }
        }"""))
    }

    "Workflow with a script" in {
      testJson[Workflow](
        Workflow(
          WorkflowPath("/TEST") ~ "VERSION",
          Vector(
            Execute.Named(WorkflowJob.Name("EXECUTABLE")),
            Execute.Named(WorkflowJob.Name("OWN-SCRIPT"))),
          Map(
            WorkflowJob.Name("EXECUTABLE") ->
              WorkflowJob(
                AgentName("AGENT"),
                ExecutablePath("/EXECUTABLE")),
            WorkflowJob.Name("OWN-SCRIPT") ->
              WorkflowJob(
                AgentName("AGENT"),
                ExecutableScript("#!/usr/bin/env bash\n...")))),
        json"""{
          "path": "/TEST",
          "versionId": "VERSION",
          "instructions": [
            {
              "TYPE": "Execute.Named",
              "jobName": "EXECUTABLE"
            }, {
              "TYPE": "Execute.Named",
              "jobName": "OWN-SCRIPT"
            }
          ],
          "jobs": {
            "EXECUTABLE": {
              "agentName": "AGENT",
              "executable": {
                "TYPE": "ExecutablePath",
                "path": "/EXECUTABLE"
              },
              "taskLimit": 1
            },
            "OWN-SCRIPT": {
              "agentName": "AGENT",
              "executable": {
                "TYPE": "ExecutableScript",
                "script": "#!/usr/bin/env bash\n..."
              },
              "taskLimit": 1
            }
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
                "agentName": "AGENT",
                "executable": {
                  "TYPE": "ExecutablePath",
                  "path": "/A.cmd"
                },
                "taskLimit": 3,
                "defaultArguments": {
                  "JOB_A": "A-VALUE"
                }
              }
            }
          ]
        }""")
    }

    "Single shell script Workflow" in {
      val workflow = Workflow(
        WorkflowPath("/WORKFLOW") ~ VersionId("1"),
        Vector(
          Execute(WorkflowJob(AgentName("AGENT"), ExecutableScript("echo HELLO\n")))))
      testJson(workflow,json"""
        {
          "instructions": [
            {
              "TYPE": "Execute.Anonymous",
              "job": {
                "agentName": "AGENT",
                "executable": {
                  "TYPE": "ExecutableScript",
                  "script": "echo HELLO\n"
                },
                "taskLimit": 1
              }
            }
          ],
          "path": "/WORKFLOW",
          "versionId": "1"
        }""")
    }
  }

  "positionMatchesSearch" in {
    val workflow = Workflow(WorkflowPath.NoId, Vector(
      "A" @: Execute.Named(WorkflowJob.Name("JOB-A")),
      If(Equal(LastReturnCode, NumericConstant(1)),
        thenWorkflow = Workflow.of(
          "B" @: Execute(WorkflowJob(AgentName("AGENT"), ExecutablePath("/EXECUTABLE")))))),
      Map(WorkflowJob.Name("JOB-A") -> WorkflowJob(AgentName("AGENT"), ExecutablePath("/EXECUTABLE"))))
      .completelyChecked.orThrow
    assert(workflow.positionMatchesSearch(Position(0), PositionSearch.ByLabel("A")))
    assert(!workflow.positionMatchesSearch(Position(0), PositionSearch.ByLabel("B")))
    assert(!workflow.positionMatchesSearch(Position(1), PositionSearch.ByLabel("B")))
    assert(!workflow.positionMatchesSearch(Position(99), PositionSearch.ByLabel("A")))
    assert(workflow.positionMatchesSearch(Position(0), PositionSearch.ByWorkflowJob(WorkflowJob.Name("JOB-A"))))
    assert(!workflow.positionMatchesSearch(Position(0), PositionSearch.ByWorkflowJob(WorkflowJob.Name("JOB-X"))))
    assert(workflow.positionMatchesSearch(Position(1) / Then % 0, PositionSearch.ByLabel(Label("B"))))
    assert(!workflow.positionMatchesSearch(Position(1) / Then % 0, PositionSearch.ByWorkflowJob(WorkflowJob.Name("JOB-X"))))
  }

  "labelToPosition of a branch" in {
    val workflow = Workflow.of(
      "A" @: Execute(WorkflowJob(AgentName("AGENT"), ExecutablePath("/EXECUTABLE"))),
      If(Equal(LastReturnCode, NumericConstant(1)),
        thenWorkflow = Workflow.of(
          "B" @: Execute(WorkflowJob(AgentName("AGENT"), ExecutablePath("/EXECUTABLE"))))))
      .completelyChecked.orThrow
    assert(workflow.labelToPosition(Nil, Label("A")) == Right(Position(0)))
    assert(workflow.labelToPosition(Nil, Label("UNKNOWN")) == Left(UnknownKeyProblem("Label", "UNKNOWN")))
    assert(workflow.labelToPosition(Position(1) / Then, Label("B")) == Right(Position(1) / Then % 0))
  }

  "labelToPosition of whole workflow" in {
    val workflow = Workflow.of(
      "A" @: Execute(WorkflowJob(AgentName("AGENT"), ExecutablePath("/EXECUTABLE"))),
      If(Equal(LastReturnCode, NumericConstant(1)),
        thenWorkflow = Workflow.of(
          "B" @: Execute(WorkflowJob(AgentName("AGENT"), ExecutablePath("/EXECUTABLE"))))))
      .completelyChecked.orThrow
    assert(workflow.labelToPosition(Label("A")) == Right(Position(0)))
    assert(workflow.labelToPosition(Label("B")) == Right(Position(1) / Then % 0))
    assert(workflow.labelToPosition(Label("UNKNOWN")) == Left(UnknownKeyProblem("Label", "UNKNOWN")))
  }

  "Duplicate labels" in {
    assert(Workflow.of(
      "DUPLICATE" @: Execute(WorkflowJob(AgentName("AGENT"), ExecutablePath("/EXECUTABLE"))),
      If(Equal(LastReturnCode, NumericConstant(1)),
        thenWorkflow = Workflow.of(
          "DUPLICATE" @: Execute(WorkflowJob(AgentName("AGENT"), ExecutablePath("/EXECUTABLE"))))))
      .completelyChecked ==
      Left(Problem("Label 'DUPLICATE' is duplicated at positions 0, 1/then:0")))
  }

  "Duplicate label in nested workflow" in {
    assert(intercept[RuntimeException] {
      Workflow.of(
        "A" @: Execute(WorkflowJob(AgentName("AGENT"), ExecutablePath("/EXECUTABLE"))),
        "A" @: Execute(WorkflowJob(AgentName("AGENT"), ExecutablePath("/EXECUTABLE"))))
    }
    .toString contains "Duplicate labels")
  }

  "Missing Label for Goto" in {
    intercept[RuntimeException] {
      Workflow.of(Goto(Label("A")))
    }
  }

  "Missing Label for IfFailedGoto" in {
    intercept[RuntimeException] {
      Workflow.of(IfFailedGoto(Label("A")))
    }
  }

  "jobOption" in {
    assert(TestWorkflow.checkedExecute(Position(0)) == Right(AExecute))
    assert(TestWorkflow.checkedExecute(Position(1)) == Left(Problem("Expected 'Execute' statement at workflow position 1 (not: If)")))
    assert(TestWorkflow.checkedExecute(Position(2)) == Left(Problem("Expected 'Execute' statement at workflow position 2 (not: Fork)")))
    assert(TestWorkflow.checkedExecute(Position(3)) == Right(BExecute))
    assert(TestWorkflow.checkedExecute(Position(4)) == Left(Problem("Expected 'Execute' statement at workflow position 4 (not: ImplicitEnd)")))
    assert(TestWorkflow.checkedExecute(Position(999)) == Left(Problem("Expected 'Execute' statement at workflow position 999 (not: Gap)")))
  }

  "workflowOption" in {
    assert(TestWorkflow.nestedWorkflow(Nil) == Right(TestWorkflow))
    assert(TestWorkflow.nestedWorkflow(Position(2) / "fork+🥕") == Right(
      TestWorkflow.instruction(2).asInstanceOf[Fork].workflow(BranchId("fork+🥕")).orThrow))
  }

  "reduce" in {
    val job = Execute(WorkflowJob(AgentName("AGENT"), ExecutablePath("/EXECUTABLE-A")))
    val B = Label("B")
    val C = Label("C")
    val D = Label("D")
    val END = Label("END")

    val instructions = Vector[(Instruction.Labeled, Boolean)](
      (()  @: job)              -> true,
      (()  @: Goto(B))          -> true,
      (C   @: job)              -> true,
      (()  @: Goto(D))          -> true,   // reducible?
      (()  @: IfFailedGoto(D))  -> false,  // reducible
      (()  @: Goto(D))          -> false,  // reducible
      (D   @: job)              -> true,
      (()  @: Goto(END))        -> false,  // reducible
      (END @: ExplicitEnd())      -> true,
      (B   @: job)              -> true,
      (()  @: Goto(C))          -> true)
    val id = WorkflowPath("/WORKFLOW") ~ "VERSION"
    val a = Workflow(id, instructions.map(_._1))
    assert(a.reduce == Workflow(id, instructions collect { case (s, true) => s }))
  }

  "numberedInstruction" in {
    assert(TestWorkflow.numberedInstructions == Vector[(InstructionNr, Instruction.Labeled)](
      (InstructionNr(0), AExecute),
      (InstructionNr(1), "TEST-LABEL" @: TestWorkflow.instruction(1)),
      (InstructionNr(2), TestWorkflow.instruction(2)),
      (InstructionNr(3), BExecute),
      (InstructionNr(4), ImplicitEnd())))
  }

  "flattendWorkflows" in {
    assert(TestWorkflow.flattenedBranchToWorkflow == Map(
      Nil -> TestWorkflow,
      (Position(1) / Then) -> TestWorkflow.instruction(Position(1)).asInstanceOf[If].thenWorkflow,
      (Position(1) / Else) -> TestWorkflow.instruction(Position(1)).asInstanceOf[If].elseWorkflow.get,
      (Position(2) / "fork+🥕") -> TestWorkflow.instruction(Position(2)).asInstanceOf[Fork].branches(0).workflow,
      (Position(2) / "fork+🍋") -> TestWorkflow.instruction(Position(2)).asInstanceOf[Fork].branches(1).workflow,
    ))
  }

  "flattenedInstruction" in {
    assert(TestWorkflow.flattenedInstructions == Vector[(Position, Instruction.Labeled)](
      (Position(0), AExecute),
      (Position(1), "TEST-LABEL" @: TestWorkflow.instruction(1)),
      (Position(1) / Then % 0, TestWorkflow.instruction(1).asInstanceOf[If].thenWorkflow.instructions(0)),
      (Position(1) / Then % 1, TestWorkflow.instruction(1).asInstanceOf[If].thenWorkflow.instructions(1)),
      (Position(1) / Then % 2, ImplicitEnd()),
      (Position(1) / Else % 0, TestWorkflow.instruction(1).asInstanceOf[If].elseWorkflow.get.instructions(0)),
      (Position(1) / Else % 1, ImplicitEnd()),
      (Position(2), TestWorkflow.instruction(2)),
      (Position(2) / "fork+🥕" % 0, TestWorkflow.instruction(2).asInstanceOf[Fork].branches(0).workflow.instructions(0)),
      (Position(2) / "fork+🥕" % 1, TestWorkflow.instruction(2).asInstanceOf[Fork].branches(0).workflow.instructions(1)),
      (Position(2) / "fork+🥕" % 2, ImplicitEnd()),
      (Position(2) / "fork+🍋" % 0, TestWorkflow.instruction(2).asInstanceOf[Fork].branches(1).workflow.instructions(0)),
      (Position(2) / "fork+🍋" % 1, TestWorkflow.instruction(2).asInstanceOf[Fork].branches(1).workflow.instructions(1)),
      (Position(2) / "fork+🍋" % 2, ImplicitEnd()),
      (Position(3), BExecute),
      (Position(4), ImplicitEnd())))
  }

  "completelyChecked in {" - {
    val wrongWorkflow = Workflow(
      WorkflowPath.NoId,
      Vector(
        If(BooleanConstant(true),
          Workflow.of(
            Execute.Named(AJobName/*undefined*/)))))
    "Unknown job" in {
      assert(wrongWorkflow.completelyChecked == Left(Problem("known job name ('A' is unknown)")))
    }

    "Known job" in {
      val workflow = wrongWorkflow.copy(nameToJob = Map(AJobName -> AJob))
      assert(workflow.completelyChecked == Right(workflow))
    }

    "retry is not allowed outside a catch block" - {
      "simple case" in {
        assert(Workflow.of(Retry()).completelyChecked == Left(Problem("Statement 'retry' is only allowed in a catch block")))
      }
    }

    "in try" in {
      assert(Workflow.of(TryInstruction(Workflow.of(Retry()), Workflow.empty)).completelyChecked
        == Left(Problem("Statement 'retry' is only allowed in a catch block")))
    }

    "in catch" in {
      assert(Workflow.of(TryInstruction(Workflow.empty, Workflow.of(Retry()))).completelyChecked.isRight)
    }

    "'if' in catch" in {
      assert(Workflow.of(TryInstruction(Workflow.empty, Workflow.of(If(BooleanConstant(true), Workflow.of(Retry())))))
        .completelyChecked.isRight)
    }

    "'fork' is a barrier" in {
      assert(Workflow.of(TryInstruction(Workflow.empty, Workflow.of(Fork.of("A" -> Workflow.of(Retry())))))
        .completelyChecked == Left(Problem("Statement 'retry' is only allowed in a catch block")))
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
      JobKey(TestWorkflow.id /: (Position(2) / "fork+🥕" % 0)) -> AExecute.job,
      JobKey(TestWorkflow.id /: (Position(2) / "fork+🍋" % 0)) -> BExecute.job,
      JobKey(TestWorkflow.id /: Position(3)) -> BExecute.job,
      JobKey(TestWorkflow.id, AJobName) -> AJob,
      JobKey(TestWorkflow.id, BJobName) -> BJob))
  }

  "anonymousJobKey" in {
    val w = Workflow(
      WorkflowPath("/TEST") ~ "VERSION",
      Vector(
        If(BooleanConstant(true),     // :0
          Workflow.of(Fail()),        // :0/then:0
          Some(Workflow.of(Fail()))), // :0/else:0
        TryInstruction(               // :1
          Workflow.of(Fail()),        // :1/Try:0
          Workflow.of(Fail()))))      // :1/1:0
    assert(w.anonymousJobKey(w.id /: Position(99)) == JobKey.Anonymous(w.id /: Position(99)))
    assert(w.anonymousJobKey(w.id /: (Position(0) / Then % 0)) == JobKey.Anonymous(w.id /: (Position(0) / Then % 0)))
    assert(w.anonymousJobKey(w.id /: (Position(0) / Else % 0)) == JobKey.Anonymous(w.id /: (Position(0) / Else % 0)))
    assert(w.anonymousJobKey(w.id /: (Position(1) / try_(0) % 0)) == JobKey.Anonymous(w.id /: (Position(1) / Try_ % 0)))
    // anonymousJobKey normalizes the retry-index of a Retry Position to 0.
    assert(w.anonymousJobKey(w.id /: (Position(1) / try_(1)    % 0)) == JobKey.Anonymous(w.id /: (Position(1) / Try_ % 0)))
  }

  "isDefinedAt, instruction" in {
    val addressToInstruction = List(
      Position(0) -> AExecute,
      Position(1) -> TestWorkflow.instruction(1),
      Position(1) / Then % 0 -> Execute.Named(AJobName),
      Position(1) / Then % 1 -> Execute.Named(BJobName),
      Position(1) / Else % 0 -> BExecute,
      Position(2) -> TestWorkflow.instruction(2),
      Position(2) / "fork+🥕" % 0 -> AExecute,
      Position(2) / "fork+🥕" % 1 -> Execute.Named(AJobName),
      Position(2) / "fork+🥕" % 2 -> ImplicitEnd(),
      Position(2) / "fork+🍋" % 0 -> BExecute,
      Position(2) / "fork+🍋" % 1 -> Execute.Named(BJobName),
      Position(2) / "fork+🍋" % 2 -> ImplicitEnd(),
      Position(3) -> BExecute,
      Position(4) -> ImplicitEnd())

    for ((address, instruction) <- addressToInstruction) {
      assert(TestWorkflow isDefinedAt address)
      assert(TestWorkflow.instruction(address) == instruction, s" - $address")
    }
    assert(!TestWorkflow.isDefinedAt(Position(0) / "fork+🥕" % 0))
    assert(!TestWorkflow.isDefinedAt(Position(0) / "fork+🥕" % 3))
    assert(!TestWorkflow.isDefinedAt(Position(999)))
  }

  "findCatchPosition" in {
    assert(TestWorkflow.findCatchPosition(Position(1) / catch_(0) % 0).isEmpty)

    val tryWorkflow = Workflow(
      WorkflowPath("/TEST") ~ "VERSION",
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
    val catch0  = Position(0) / catch_(0) % 0
    val catch1  = Position(1) / catch_(0) % 0
    val catch10 = Position(1) / try_(0)   % 0 / catch_(0) % 0
    val catch11 = Position(1) / catch_(0) % 0 / catch_(0) % 0

    assert(tryWorkflow.findCatchPosition(Position(0)           ) == None)
    assert(tryWorkflow.findCatchPosition(Position(0) / try_(0) % 0) == Some(catch0))
    assert(tryWorkflow.findCatchPosition(Position(0) / try_(0) % 1) == Some(catch0))
    assert(tryWorkflow.findCatchPosition(catch0                ) == None)
    assert(tryWorkflow.findCatchPosition(catch0.increment      ) == None)

    assert(tryWorkflow.findCatchPosition(Position(1)                      ) == None)
    assert(tryWorkflow.findCatchPosition(Position(1) / try_(0) % 0           ) == Some(catch1))
    assert(tryWorkflow.findCatchPosition(Position(1) / try_(0) % 0 / try_(0) % 0) == Some(catch10))
    assert(tryWorkflow.findCatchPosition(Position(1) / try_(0) % 0 / try_(0) % 1) == Some(catch10))
    assert(tryWorkflow.findCatchPosition(catch10                          ) == Some(catch1))
    assert(tryWorkflow.findCatchPosition(catch10.increment                ) == Some(catch1))
    assert(tryWorkflow.findCatchPosition(Position(1) / try_(0) % 1           ) == Some(catch1))

    assert(tryWorkflow.findCatchPosition(Position(1) / catch_(0) % 0 / try_(0) % 0) == Some(catch11))
    assert(tryWorkflow.findCatchPosition(Position(1) / catch_(0) % 0 / try_(0) % 1) == Some(catch11))
    assert(tryWorkflow.findCatchPosition(catch11          ) == None)
    assert(tryWorkflow.findCatchPosition(catch11.increment) == None)
    assert(tryWorkflow.findCatchPosition(catch1           ) == None)
    assert(tryWorkflow.findCatchPosition(catch1.increment ) == None)

    assert(tryWorkflow.findCatchPosition(Position(2)) == None)
  }

  "findCatchPosition in fork, an exception barrier" in {
    assert(TestWorkflow.findCatchPosition(Position(1) / catch_(0) % 0).isEmpty)

    val tryWorkflow = Workflow(
      WorkflowPath("/TEST") ~ "VERSION",
      Vector(
        TryInstruction(                              // :0
          tryWorkflow = Workflow.of(
            Fork.of(                                 // :0/0:0
              "🍋" -> Workflow.of(AExecute))),       // :0/0:0/0:0
          catchWorkflow = Workflow.of(BExecute))))   // :0/0:0/1:0      catch0
    val forkPosition = Position(0) / try_(0) % 0
    assert(tryWorkflow.instruction(forkPosition).isInstanceOf[Fork])
    assert(tryWorkflow.findCatchPosition(forkPosition) == Some(Position(0) / catch_(0) % 0))

    val executePosition = forkPosition / "fork+🍋" % 0
    assert(tryWorkflow.instruction(executePosition).isInstanceOf[Execute])
    assert(tryWorkflow.findCatchPosition(executePosition) == None)  // Do not escape Fork!
  }

  "isMoveable" - {
    "Same BranchPath is okay" in {
      for (i <- TestWorkflow.instructions.indices; j <- TestWorkflow.instructions.indices) {
        assert(TestWorkflow.isMoveable(Position(i), Position(j)))
      }
    }

    "Undefined Position is not okay" in {
      assert(!TestWorkflow.isMoveable(Position(0), Position(TestWorkflow.instructions.length)))
      assert(!TestWorkflow.isMoveable(Position(TestWorkflow.instructions.length), Position(0)))
    }

    "if-then-else is okay" in {
      assert(TestWorkflow.isMoveable(Position(0), Position(1) / Then % 0))
      assert(TestWorkflow.isMoveable(Position(1), Position(1) / Then % 0))
      assert(TestWorkflow.isMoveable(Position(1), Position(1) / Then % 1))
      assert(TestWorkflow.isMoveable(Position(1), Position(1) / Then % 0))
      assert(TestWorkflow.isMoveable(Position(1), Position(1) / Else % 1))
    }

    "Different fork branches are not okay" in {
      assert(!TestWorkflow.isMoveable(Position(2), Position(2) / fork("🥕") % 0))
      assert(!TestWorkflow.isMoveable(Position(2) / fork("🥕") % 0, Position(2) / fork("🍋") % 0))
    }

    "Same fork branch is okay" in {
      assert(TestWorkflow.isMoveable(Position(2) / fork("🥕") % 0, Position(2) / fork("🥕") % 0))
      assert(TestWorkflow.isMoveable(Position(2) / fork("🥕") % 0, Position(2) / fork("🥕") % 2))
    }

    "try catch is okay" in {
      val workflow = {
        val execute = Execute.Anonymous(WorkflowJob(AgentName("AGENT"), ExecutablePath("/SCRIPT")))
        Workflow.of(
          execute,
          TryInstruction(
            tryWorkflow = Workflow.of(execute),
            catchWorkflow = Workflow.of(execute)),
          execute)
      }
      assert(workflow.isMoveable(Position(1), Position(1) / Try_ % 0))
      assert(workflow.isMoveable(Position(1), Position(1) / Catch_ % 0))
      assert(workflow.isMoveable(Position(1) / Catch_ % 0, Position(1)))
    }
  }
}

private object WorkflowTest
{
  private val TestWorkflow = Workflow(
    WorkflowPath("/TEST") ~ "VERSION",
    Vector(
      AExecute,
      "TEST-LABEL" @: If(Equal(LastReturnCode, NumericConstant(1)),
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
