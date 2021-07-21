package js7.data.workflow

import io.circe.syntax._
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceEither}
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.problem.Problems.UnknownKeyProblem
import js7.data.agent.AgentPath
import js7.data.item.VersionId
import js7.data.job.{JobKey, JobResourcePath, PathExecutable, ShellScriptExecutable}
import js7.data.lock.LockPath
import js7.data.value.NumberValue
import js7.data.value.expression.Expression.{BooleanConstant, Equal, JobResourceVariable, LastReturnCode, NumericConstant, StringConstant}
import js7.data.value.expression.PositionSearch
import js7.data.workflow.WorkflowTest._
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ExplicitEnd, Fail, Fork, Gap, Goto, If, IfFailedGoto, ImplicitEnd, LockInstruction, Retry, TryInstruction}
import js7.data.workflow.position.BranchId.{Catch_, Else, Then, Try_, fork, try_}
import js7.data.workflow.position._
import js7.data.workflow.test.ForkTestSetting
import js7.data.workflow.test.TestSetting._
import js7.tester.CirceJsonTester.{normalizeJson, removeJNull, testJson}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowTest extends AnyFreeSpec
{
  "JSON" - {
    "Minimum readable Workflow" in {
      val json = json"""{
        "path": "WORKFLOW",
        "versionId": "VERSION",
        "instructions": []
      }"""
      assert(json.as[Workflow].toChecked.orThrow ==
        Workflow(WorkflowPath("WORKFLOW") ~ "VERSION", Nil))
    }

    "Workflow" in {
      testJson[Workflow](TestWorkflow,
        json"""{
          "path": "TEST",
          "versionId": "VERSION",
          "orderPreparation": {
            "parameters": {
              "myRequired": {
                "type": "Number"
              },
              "myOptional": {
                "type": "String",
                "default": "'DEFAULT'"
              },
              "myFinal": {
                "final": "'FINAL'"
              }
            },
            "allowUndeclared": true
          },
          "instructions": [
            {
              "TYPE": "Execute.Anonymous",
              "job": {
                "agentPath": "AGENT",
                "executable": {
                  "TYPE": "PathExecutable",
                  "v1Compatible": true,
                  "path": "A.cmd"
                },
                "parallelism": 3,
                "defaultArguments": { "JOB_A": "'A-VALUE'" }
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
                    "agentPath": "AGENT",
                    "executable": {
                      "TYPE": "PathExecutable",
                      "v1Compatible": true,
                      "path": "B.cmd"
                    },
                    "parallelism": 3 ,
                    "defaultArguments": { "JOB_B1": "'B1-VALUE'" }
                  }
                }
              },
              "else": {
                "instructions": [
                  {
                    "TYPE": "Execute.Anonymous",
                    "job": {
                      "agentPath": "AGENT",
                      "executable": {
                        "TYPE": "PathExecutable",
                        "v1Compatible": true,
                        "path": "B.cmd"
                      },
                      "parallelism": 3,
                      "defaultArguments": { "JOB_B": "'B-VALUE'" }
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
                          "agentPath": "AGENT",
                          "executable": {
                            "TYPE": "PathExecutable",
                            "v1Compatible": true,
                            "path": "A.cmd"
                          },
                          "parallelism": 3,
                          "defaultArguments": { "JOB_A": "'A-VALUE'" }
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
                          "agentPath": "AGENT",
                          "executable": {
                            "TYPE": "PathExecutable",
                            "v1Compatible": true,
                            "path": "B.cmd"
                          },
                          "parallelism": 3,
                          "defaultArguments": { "JOB_B": "'B-VALUE'" }
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
                "agentPath": "AGENT",
                "executable": {
                  "TYPE": "PathExecutable",
                  "v1Compatible": true,
                  "path": "B.cmd"
                },
                "parallelism": 3,
                "defaultArguments": { "JOB_B": "'B-VALUE'" }
              }
            }
          ],
          "jobs": {
            "A": {
              "agentPath": "AGENT",
              "executable": {
                "TYPE": "PathExecutable",
                "v1Compatible": true,
                "path": "A.cmd"
              },
              "parallelism": 3,
              "defaultArguments": { "JOB_A": "'A-VALUE'" }
            },
            "B": {
              "agentPath": "AGENT",
              "executable": {
                "TYPE": "PathExecutable",
                "v1Compatible": true,
                "path": "B.cmd"
              },
              "parallelism": 3,
              "defaultArguments": { "JOB_B": "'B-VALUE'" }}
          },
          "jobResourcePaths": [ "JOB-RESOURCE" ]
        }""")
    }

    "Workflow with positions (for JOC GUI)" in {
      assert(normalizeJson(removeJNull(TestWorkflow.withPositions(Nil).asJson)) ==
        normalizeJson(json"""{
          "path": "TEST",
          "versionId": "VERSION",
          "orderPreparation": {
            "parameters": {
              "myRequired": {
                "type": "Number"
              },
              "myOptional": {
                "type": "String",
                "default": "'DEFAULT'"
              },
              "myFinal": {
                "final": "'FINAL'"
              }
            },
            "allowUndeclared": true
          },
          "instructions": [
            {
              "position": [ 0 ],
              "TYPE": "Execute.Anonymous",
              "job": {
                "agentPath": "AGENT",
                "executable": {
                  "TYPE": "PathExecutable",
                  "v1Compatible": true,
                  "path": "A.cmd"
                },
                "parallelism": 3,
                "defaultArguments": { "JOB_A": "'A-VALUE'" }
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
                    "agentPath": "AGENT",
                    "executable": {
                      "TYPE": "PathExecutable",
                      "v1Compatible": true,
                      "path": "B.cmd"
                    },
                    "parallelism": 3 ,
                    "defaultArguments": { "JOB_B1": "'B1-VALUE'" }
                  }
                }
              },
              "else": {
                "instructions": [
                  {
                    "position": [ 1, "else", 0 ],
                    "TYPE": "Execute.Anonymous",
                    "job": {
                      "agentPath": "AGENT",
                      "executable": {
                        "TYPE": "PathExecutable",
                        "v1Compatible": true,
                        "path": "B.cmd"
                      },
                      "parallelism": 3,
                      "defaultArguments": { "JOB_B": "'B-VALUE'" }
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
                          "agentPath": "AGENT",
                          "executable": {
                            "TYPE": "PathExecutable",
                            "v1Compatible": true,
                            "path": "A.cmd"
                          },
                          "parallelism": 3,
                          "defaultArguments": { "JOB_A": "'A-VALUE'" }
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
                          "agentPath": "AGENT",
                          "executable": {
                            "TYPE": "PathExecutable",
                            "v1Compatible": true,
                            "path": "B.cmd"
                          },
                          "parallelism": 3,
                          "defaultArguments": { "JOB_B": "'B-VALUE'" }
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
                "agentPath": "AGENT",
                "executable": {
                  "TYPE": "PathExecutable",
                  "v1Compatible": true,
                  "path": "B.cmd"
                },
                "parallelism": 3,
                "defaultArguments": { "JOB_B": "'B-VALUE'" }
              }
            }, {
              "position": [ 4 ],
              "TYPE": "ImplicitEnd"
            }
          ],
          "jobs": {
            "A": {
              "agentPath": "AGENT",
              "executable": {
                "TYPE": "PathExecutable",
                "v1Compatible": true,
                "path": "A.cmd"
              },
              "parallelism": 3,
              "defaultArguments": { "JOB_A": "'A-VALUE'" }
            },
            "B": {
              "agentPath": "AGENT",
              "executable": {
                "TYPE": "PathExecutable",
                "v1Compatible": true,
                "path": "B.cmd"
              },
              "parallelism": 3,
              "defaultArguments": { "JOB_B": "'B-VALUE'" }}
          },
          "jobResourcePaths": [ "JOB-RESOURCE" ]
        }"""))
    }

    "Workflow with a script" in {
      testJson[Workflow](
        Workflow(
          WorkflowPath("TEST") ~ "VERSION",
          Vector(
            Execute.Named(WorkflowJob.Name("EXECUTABLE")),
            Execute.Named(WorkflowJob.Name("OWN-SCRIPT"))),
          Map(
            WorkflowJob.Name("EXECUTABLE") ->
              WorkflowJob(
                AgentPath("AGENT"),
                PathExecutable("EXECUTABLE")),
            WorkflowJob.Name("OWN-SCRIPT") ->
              WorkflowJob(
                AgentPath("AGENT"),
                ShellScriptExecutable("#!/usr/bin/env bash\n...")))),
        json"""{
          "path": "TEST",
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
              "agentPath": "AGENT",
              "executable": {
                "TYPE": "PathExecutable",
                "path": "EXECUTABLE"
              },
              "parallelism": 1
            },
            "OWN-SCRIPT": {
              "agentPath": "AGENT",
              "executable": {
                "TYPE": "ShellScriptExecutable",
                "script": "#!/usr/bin/env bash\n..."
              },
              "parallelism": 1
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
                "agentPath": "AGENT",
                "executable": {
                  "TYPE": "PathExecutable",
                  "v1Compatible": true,
                  "path": "A.cmd"
                },
                "parallelism": 3,
                "defaultArguments": {
                  "JOB_A": "'A-VALUE'"
                }
              }
            }
          ]
        }""")
    }

    "Single shell script Workflow" in {
      val workflow = Workflow(
        WorkflowPath("WORKFLOW") ~ VersionId("1"),
        Vector(
          Execute(WorkflowJob(AgentPath("AGENT"), ShellScriptExecutable("echo HELLO\n")))))
      testJson(workflow,json"""
        {
          "instructions": [
            {
              "TYPE": "Execute.Anonymous",
              "job": {
                "agentPath": "AGENT",
                "executable": {
                  "TYPE": "ShellScriptExecutable",
                  "script": "echo HELLO\n"
                },
                "parallelism": 1
              }
            }
          ],
          "path": "WORKFLOW",
          "versionId": "1"
        }""")
    }
  }

  "positionMatchesSearch" in {
    val workflow = Workflow(WorkflowPath.NoId, Vector(
      "A" @: Execute.Named(WorkflowJob.Name("JOB-A")),
      If(Equal(LastReturnCode, NumericConstant(1)),
        thenWorkflow = Workflow.of(
          "B" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE")))))),
      Map(WorkflowJob.Name("JOB-A") -> WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE"))))
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
      "A" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE"))),
      If(Equal(LastReturnCode, NumericConstant(1)),
        thenWorkflow = Workflow.of(
          "B" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE"))))))
      .completelyChecked.orThrow
    assert(workflow.labelToPosition(Nil, Label("A")) == Right(Position(0)))
    assert(workflow.labelToPosition(Nil, Label("UNKNOWN")) == Left(UnknownKeyProblem("Label", "UNKNOWN")))
    assert(workflow.labelToPosition(Position(1) / Then, Label("B")) == Right(Position(1) / Then % 0))
  }

  "labelToPosition of whole workflow" in {
    val workflow = Workflow.of(
      "A" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE"))),
      If(Equal(LastReturnCode, NumericConstant(1)),
        thenWorkflow = Workflow.of(
          "B" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE"))))))
      .completelyChecked.orThrow
    assert(workflow.labelToPosition(Label("A")) == Right(Position(0)))
    assert(workflow.labelToPosition(Label("B")) == Right(Position(1) / Then % 0))
    assert(workflow.labelToPosition(Label("UNKNOWN")) == Left(UnknownKeyProblem("Label", "UNKNOWN")))
  }

  "Duplicate labels" in {
    assert(Workflow.of(
      "DUPLICATE" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE"))),
      If(Equal(LastReturnCode, NumericConstant(1)),
        thenWorkflow = Workflow.of(
          "DUPLICATE" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE"))))))
      .completelyChecked ==
      Left(Problem("Label 'DUPLICATE' is duplicated at positions 0, 1/then:0")))
  }

  "Duplicate label in nested workflow" in {
    assert(intercept[RuntimeException] {
      Workflow.of(
        "A" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE"))),
        "A" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE"))))
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
    val job = Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE-A")))
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
    val id = WorkflowPath("WORKFLOW") ~ "VERSION"
    val a = Workflow(id, instructions.map(_._1))
    assert(a.reduce == Workflow(id, instructions collect { case (s, true) => s }))
  }

  "numberedInstruction" in {
    assert(TestWorkflow.numberedInstructions.toSeq == Seq[(InstructionNr, Instruction.Labeled)](
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
    assert(TestWorkflow.flattenedInstructions.toSeq == Seq[(Position, Instruction.Labeled)](
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

  "checkedPosition" in {
    assert(TestWorkflow.checkedPosition(Position(0)) == Right(Position(0)))
    assert(TestWorkflow.checkedPosition(Position(1) / Then % 0) == Right(Position(1) / Then % 0))
    assert(TestWorkflow.checkedPosition(Position(2) / "fork+🥕" % 2) == Right(Position(2) / "fork+🥕" % 2))
    assert(TestWorkflow.checkedPosition(Position(2) / "fork+🥕" % 3) == Left(Problem("Unknown position 2/fork+🥕:3 in workflow 'Workflow:TEST~VERSION'")))
    assert(TestWorkflow.checkedPosition(Position(5)) == Left(Problem("Unknown position 5 in workflow 'Workflow:TEST~VERSION'")))
  }

  "completelyChecked in {" - {
    val wrongWorkflow = Workflow(
      WorkflowPath.NoId,
      Vector(
        If(BooleanConstant(true),
          Workflow.of(
            Execute.Named(AJobName/*undefined*/)))))
    "Unknown job" in {
      assert(wrongWorkflow.completelyChecked == Left(Problem("Unknown job name 'A'")))
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

  "referencedLockPaths" in {
    val job = WorkflowJob(AgentPath("AGENT"), ShellScriptExecutable(""))
    val a = LockPath("A")
    val b = LockPath("B")
    val c = LockPath("C")
    val workflow = Workflow(
      WorkflowPath("WORKFLOW") ~ "1",
      Vector(
        LockInstruction(a, None, Workflow.of(
          Execute(job),
          If(BooleanConstant(true), Workflow.of(
            LockInstruction(b, None, Workflow.of(
            Execute(job),
            Fork.of(
              "BRANCH" -> Workflow.of(
                LockInstruction(c, None, Workflow.of(
                  Execute(job)))))))))))))
    assert(workflow.referencedLockPaths == Set(a, b, c))
    assert(workflow.referencedAgentPaths == Set(AgentPath("AGENT")))
    assert(workflow.referencedJobResourcePaths.isEmpty)
  }

  "referencedAgentPaths" in {
    val a = AgentPath("A")
    val b = AgentPath("B")
    val c = AgentPath("C")
    val d = AgentPath("D")
    val aJob = WorkflowJob(a, ShellScriptExecutable(""))
    val bJob = WorkflowJob(b, ShellScriptExecutable(""))
    val cJob = WorkflowJob(c, ShellScriptExecutable(""))
    val dJob = WorkflowJob(d, ShellScriptExecutable(""))
    val workflow = Workflow(
      WorkflowPath("WORKFLOW") ~ "1",
      Vector(
        Execute(aJob),
        If(BooleanConstant(true), Workflow.of(
          Execute(bJob),
          Fork.of(
            "BRANCH" -> Workflow.of(
              Execute(cJob)))))),
      Map(
        WorkflowJob.Name("D") -> dJob))
    assert(workflow.referencedLockPaths.isEmpty)
    assert(workflow.referencedAgentPaths == Set(a, b, c, d))
    assert(workflow.referencedJobResourcePaths.isEmpty)
    assert(workflow.workflowJobs.toSet == Set(aJob, bJob, cJob, dJob))
  }

  "referencedJobResourcePaths" in {
    val a = JobResourcePath("A")
    val b = JobResourcePath("B")
    val c = JobResourcePath("C")
    val d = JobResourcePath("D")
    val e = JobResourcePath("E")
    val f = JobResourcePath("F")
    val job = WorkflowJob(
      AgentPath("AGENT"),
      ShellScriptExecutable("", env = Map("X" -> JobResourceVariable(e, Some("SETTING")))))
    val workflow = Workflow(
      WorkflowPath("WORKFLOW") ~ "1",
      Vector(
        Execute(job.copy(jobResourcePaths = Seq(a))),
        If(BooleanConstant(true), Workflow.of(
          Execute(job.copy(jobResourcePaths = Seq(b, c))),
          Fork.of(
            "BRANCH" -> Workflow.of(
              Execute(job.copy(jobResourcePaths = Seq(c, d)))))))),
      orderPreparation = OrderPreparation(OrderParameters(
        OrderParameter.Final("V", JobResourceVariable(f, Some("V"))))))
    assert(workflow.referencedLockPaths.isEmpty)
    assert(workflow.referencedAgentPaths == Set(AgentPath("AGENT")))
    assert(workflow.referencedJobResourcePaths == Set(a, b, c, d, e, f))
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
      WorkflowPath("TEST") ~ "VERSION",
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

  "isMoveable, reachablePositions" - {
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

      assert(TestWorkflow.isMoveable(Position(1) / Then % 0, Position(0)))
      assert(TestWorkflow.isMoveable(Position(1), Position(1) / Then % 0))
      assert(TestWorkflow.isMoveable(Position(1), Position(1) / Then % 1))
      assert(TestWorkflow.isMoveable(Position(1) / Then % 0, Position(1)))
      assert(TestWorkflow.isMoveable(Position(1) / Then % 1, Position(1)))

      assert(TestWorkflow.isMoveable(Position(1), Position(1) / Else % 0))
      assert(TestWorkflow.isMoveable(Position(1) / Else % 0, Position(1)))

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

    "Lock" - {
      lazy val lockWorkflow = Workflow(WorkflowPath("TEST") ~ "1",
        Vector(
          AExecute,
          LockInstruction(LockPath("LOCK"), None, Workflow.of(
            If(BooleanConstant(true),
              Workflow.of(AExecute))))),
        Map(
          AJobName -> AJob))

      "Into same locked block" in {
        assert(lockWorkflow.isMoveable(Position(1) / BranchId.Lock % 0, Position(1) / BranchId.Lock % 0))
        assert(lockWorkflow.isMoveable(Position(1) / BranchId.Lock % 0, Position(1) / BranchId.Lock % 0 / Then % 0))
        assert(lockWorkflow.isMoveable(Position(1) / BranchId.Lock % 0 / Then % 0, Position(1) / BranchId.Lock % 0))
      }

      "Into a locked block is not okay" in {
        assert(!lockWorkflow.isMoveable(Position(1), Position(1) / BranchId.Lock % 0))
      }

      "Out of a locked block is not okay" in {
        assert(!lockWorkflow.isMoveable(Position(1) / BranchId.Lock % 0, Position(1)))
      }

      "reachablePositions" in {
        val reachable = Seq(
          Position(1) / BranchId.Lock % 0,
          Position(1) / BranchId.Lock % 0 / Then % 0,
          Position(1) / BranchId.Lock % 0 / Then % 1,
          Position(1) / BranchId.Lock % 1)

        for (from <- reachable) {
          assert(lockWorkflow.reachablePositions(from).toSeq == reachable)
        }
      }
    }

    "try catch" - {
      lazy val tryWorkflow = {
        val execute = Execute.Anonymous(WorkflowJob(AgentPath("AGENT"), PathExecutable("SCRIPT")))
        Workflow.of(
          execute,
          TryInstruction(
            tryWorkflow = Workflow.of(execute),
            catchWorkflow = Workflow.of(execute)),
          execute)
      }

      "try catch is okay" in {
        assert(tryWorkflow.isMoveable(Position(1), Position(1) / Try_ % 0))
        assert(tryWorkflow.isMoveable(Position(1), Position(1) / Catch_ % 0))
        assert(tryWorkflow.isMoveable(Position(1) / Catch_ % 0, Position(1)))
      }

      "reachablePositions" in {
        val reachable = Seq(
          Position(0),
          Position(1),
          Position(1) / Try_ % 0,
          Position(1) / Try_ % 1,
          Position(1) / Catch_ % 0,
          Position(1) / Catch_ % 1,
          Position(2),
          Position(3))

        for (from <- reachable) {
          assert(tryWorkflow.reachablePositions(from).toSeq == reachable)
        }
      }
    }

    "reachablePositions from first level" in {
      val reachable = Seq(
        Position(0),
        Position(1),
        Position(1) / Then % 0,
        Position(1) / Then % 1,
        Position(1) / Then % 2,
        Position(1) / Else % 0,
        Position(1) / Else % 1,
        Position(2),
        Position(3),
        Position(4))

      for (from <- reachable) {
        assert(TestWorkflow.reachablePositions(from).toSeq == reachable)
      }
    }

    "reachablePositions from fork" in {
      val reachable = Seq(
        Position(2) / fork("🥕") % 0,
        Position(2) / fork("🥕") % 1,
        Position(2) / fork("🥕") % 2)

      for (from <- reachable) {
        assert(TestWorkflow.reachablePositions(from).toSeq == reachable)
      }
    }

    // For reachablePositions for Lock and Try, see above.
  }

  "Workflow with a Lock and a Job" in {
    Workflow(WorkflowPath("WORKFLOW"),
        Vector(
          LockInstruction(
            LockPath("LOCK"), count = None, Workflow.of(
              Execute.Named(WorkflowJob.Name("JOB"))))),
        Map(
          WorkflowJob.Name("JOB") -> AJob))
      .completelyChecked
      .orThrow: Workflow
  }

  "reduceForAgent" - {
    import js7.data.workflow.test.ForkTestSetting._

    "reduceForAgent A" in {
      assert(TestWorkflow.reduceForAgent(AAgentPath) == Workflow(
        TestWorkflow.id,
        Vector(
          /*0*/ Fork.of(
            "🥕" -> Workflow.of(AExecute),
            "🍋" -> Workflow.of(AExecute)),
          /*1*/ Fork.of(
            "🥕" -> Workflow.of(AExecute),
            "🍋" -> Workflow.of(AExecute)),
          /*2*/ Gap(),
          /*3*/ Fork.of(
            "🥕" -> Workflow.of(Gap()),
            "🍋" -> Workflow.of(AExecute, Gap())),
          /*4*/ Fork.of(
            "🥕" -> Workflow.of(AExecute),
            "🍋" -> Workflow.of(Gap()))),
        Map(AJobName -> AJob),
        source = TestWorkflow.source))
    }

    "reduceForAgent B" in {
      assert(TestWorkflow.reduceForAgent(BAgentPath) == Workflow(
        WorkflowPath("WORKFLOW") ~ "INITIAL" ,
        Vector(
          /*0*/ Gap(),
          /*1*/ Gap(),
          /*2*/ BExecute,
          /*3*/ Fork.of(
            "🥕" -> Workflow.of(BExecute),
            "🍋" -> Workflow.of(Gap(), BExecute)),
          /*4*/ Fork.of(
            "🥕" -> Workflow.of(Gap()),
            "🍋" -> Workflow.of(BExecute))),
        Map(BJobName -> BJob),
        source = TestWorkflow.source))
    }

    "reduceForAgent with LockInstruction" in {
      val workflow = Workflow(WorkflowPath.Anonymous,
        Vector(
          LockInstruction(LockPath("LOCK"), count=None, Workflow.of(
            AExecute,
            Fork.of(
              "🥕" -> Workflow.of(AExecute))))),
        Map(AJobName -> AJob))
      assert(workflow.reduceForAgent(AAgentPath) eq workflow)
      assert(workflow.reduceForAgent(BAgentPath) == Workflow.of(Gap()))
    }

    "reduceForAgent with LockInstruction and more (1)" in {
      val workflow = Workflow(WorkflowPath.Anonymous,
        Vector(
          LockInstruction(LockPath("LOCK"), count=None, Workflow.of(
            If(BooleanConstant(true), Workflow.of(
              TryInstruction(Workflow.empty, Workflow.empty),
              Fail(),
            )),
            AExecute,
            Fork.of(
              "🥕" -> Workflow.of(AExecute))))),
        Map(AJobName -> AJob))
      assert(workflow.reduceForAgent(AAgentPath) eq workflow)
      assert(workflow.reduceForAgent(BAgentPath) == Workflow.of(Gap()))
    }

    "reduceForAgent with LockInstruction and if" in {
      val workflow = Workflow(WorkflowPath.Anonymous,
        Vector(
          LockInstruction(LockPath("LOCK"), count=None, Workflow.of(
            AExecute,
            If(BooleanConstant(true),
              Workflow.of(
                BExecute,
                TryInstruction(Workflow.empty, Workflow.empty),
                Fail()),
              Some(Workflow.of(
                BExecute))),
            AExecute))),
        Map(
          AJobName -> AJob,
          BJobName -> BJob))

      assert(workflow.reduceForAgent(AAgentPath) ==
        Workflow(WorkflowPath.Anonymous,
          Vector(
            LockInstruction(LockPath("LOCK"), count=None, Workflow.of(
              AExecute,
              If(BooleanConstant(true),
                Workflow.of(
                  Gap(),
                  TryInstruction(Workflow.empty, Workflow.empty),
                  Fail()),
                Some(Workflow.of(
                  Gap()))),
              AExecute))),
          Map(
            AJobName -> AJob)))

      assert(workflow.reduceForAgent(BAgentPath) ==
        Workflow(WorkflowPath.Anonymous,
          Vector(
            LockInstruction(LockPath("LOCK"), count=None, Workflow.of(
              Gap(),
              If(BooleanConstant(true),
                Workflow.of(
                  BExecute,
                  TryInstruction(Workflow.empty, Workflow.empty),
                  Fail()),
              Some(Workflow.of(
                BExecute))),
            Gap()))),
          Map(
            BJobName -> BJob)))
    }

    "isStartableOnAgent" - {
      val isStartableSetting = List(
        Position(0) -> List(AAgentPath),
        Position(0) / "fork+🥕" % 0 -> List(AAgentPath),
        Position(0) / "fork+🥕" % 1 -> Nil,
        Position(0) / "fork+🍋" % 0 -> List(AAgentPath),
        Position(0) / "fork+🍋" % 1 -> Nil,
        Position(1) -> List(AAgentPath),
        Position(1) / "fork+🥕" % 0 -> List(AAgentPath),
        Position(1) / "fork+🥕" % 1 -> Nil,
        Position(1) / "fork+🍋" % 0 -> List(AAgentPath),
        Position(1) / "fork+🍋" % 1 -> Nil,
        Position(2) -> List(BAgentPath),
        Position(3) -> List(AAgentPath, BAgentPath),
        Position(3) / "fork+🥕" % 0 -> List(BAgentPath),
        Position(3) / "fork+🥕" % 1 -> Nil,
        Position(3) / "fork+🍋" % 0 -> List(AAgentPath),
        Position(3) / "fork+🍋" % 1 -> List(BAgentPath),
        Position(3) / "fork+🍋" % 2 -> Nil,
        Position(4) -> List(AAgentPath, BAgentPath),  // Order 🍋 is created on A but executed on B
        Position(4) / "fork+🥕" % 0 -> List(AAgentPath),
        Position(4) / "fork+🥕" % 1 -> Nil,
        Position(4) / "fork+🍋" % 0 -> List(BAgentPath),
        Position(4) / "fork+🍋" % 1 -> Nil,
        Position(5) -> Nil)

      for ((position, agentPaths) <- isStartableSetting) {
        for ((agentPath, expected) <- agentPaths.map(_ -> true) ++ (AgentPaths filterNot agentPaths.toSet).map(_ -> false)) {
          s"isStartableOnAgent($position $agentPath) = $expected" in {
            assert(TestWorkflow.isStartableOnAgent(position, agentPath) == expected)
          }
          s".reduceForAgent.isStartableOnAgent($position $agentPath) = $expected" in {
            //assert(SimpleTestWorkflow.reduceForAgent(agentPath).isStartableOnAgent(position, agentPath))
            assert(TestWorkflow.reduceForAgent(agentPath).isStartableOnAgent(position, agentPath) == expected)
          }
        }
      }
    }

    //"determinedExecutingAgent" - {
    //  val setting = List(
    //    Position(0) -> Some(AAgentPath),
    //    Position(1) -> Some(AAgentPath),
    //    Position(2) -> Some(BAgentPath),
    //    Position(3) -> None,
    //    Position(4) -> None,
    //    Position(5) -> Nil)
    //
    //  for ((position, expected) <- setting) {
    //    s"determinedExecutingAgent($position)" in {
    //      assert(TestWorkflow.determinedExecutingAgent(position) == expected)
    //    }
    //  }
    //}
  }

  "referencedItempPaths" in {
    assert(TestWorkflow.referencedItemPaths.toSet == Set(
      TestAgentPath,
      JobResourcePath("JOB-RESOURCE")))

    assert(ForkTestSetting.TestWorkflow.referencedItemPaths.toSet == Set(
      ForkTestSetting.AAgentPath,
      ForkTestSetting.BAgentPath))
  }
}

private object WorkflowTest
{
  private val TestWorkflow = Workflow(
    WorkflowPath("TEST") ~ "VERSION",
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
      BJobName -> BJob),
    OrderPreparation(
      OrderParameters(
        Seq(
          OrderParameter("myRequired", NumberValue),
          OrderParameter("myOptional", StringConstant("DEFAULT")),
          OrderParameter.Final("myFinal", StringConstant("FINAL"))),
        allowUndeclared = true)),
    jobResourcePaths = Seq(
      JobResourcePath("JOB-RESOURCE")))
}
