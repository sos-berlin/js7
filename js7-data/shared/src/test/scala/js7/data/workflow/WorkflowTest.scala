package js7.data.workflow
import io.circe.syntax.*
import js7.base.circeutils.CirceUtils.{JsonStringInterpolator, RichCirceEither}
import js7.base.problem.Checked.*
import js7.base.problem.Problem
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.test.OurTestSuite
import js7.base.time.Timezone
import js7.data.agent.AgentPath
import js7.data.board.{BoardPath, BoardPathExpression}
import js7.data.calendar.CalendarPath
import js7.data.item.VersionId
import js7.data.job.{JobKey, JobResourcePath, PathExecutable, ShellScriptExecutable}
import js7.data.lock.LockPath
import js7.data.subagent.SubagentBundleId
import js7.data.value.NumberValue
import js7.data.value.expression.Expression.{BooleanConstant, Equal, JobResourceVariable, LastReturnCode, NumericConstant, StringConstant}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.expression.PositionSearch
import js7.data.workflow.WorkflowTest.*
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ExpectNotices, Fail, Fork, Gap, If, ImplicitEnd, LockInstruction, PostNotices, Retry, TryInstruction}
import js7.data.workflow.position.*
import js7.data.workflow.position.BranchId.{Catch_, Else, Then, Try_, fork, try_}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.test.ForkTestSetting
import js7.data.workflow.test.TestSetting.*
import js7.tester.CirceJsonTester.{normalizeJson, removeJNull, testJson}

/**
  * @author Joacim Zschimmer
  */
final class WorkflowTest extends OurTestSuite:

  "JSON" - {
    "Minimum readable Workflow" in:
      val json = json"""{
        "path": "WORKFLOW",
        "versionId": "VERSION",
        "instructions": []
      }"""
      assert(json.as[Workflow].toChecked.orThrow ==
        Workflow(WorkflowPath("WORKFLOW") ~ "VERSION", Nil))

    "Workflow" in:
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
          "timeZone": "Europe/Berlin",
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
                "processLimit": 3,
                "defaultArguments": { "JOB_A": "'A-VALUE'" }
              }
            }, {
              "label": "TEST-LABEL",
              "TYPE": "If",
              "ifThens": [{
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
                      "processLimit": 3 ,
                      "defaultArguments": { "JOB_B1": "'B1-VALUE'" }
                    }
                  }
                }
              }],
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
                      "processLimit": 3,
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
                          "processLimit": 3,
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
                          "processLimit": 3,
                          "defaultArguments": { "JOB_B": "'B-VALUE'" }
                        }
                      },
                      { "TYPE": "Execute.Named", "jobName": "B" }
                    ],
                    "result": {
                      "RESULT": "$$RESULT"
                    }
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
                "processLimit": 1,
                "subagentBundleIdExpr": "'SUBAGENT-BUNDLE'"
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
              "processLimit": 3,
              "defaultArguments": { "JOB_A": "'A-VALUE'" }
            },
            "B": {
              "agentPath": "AGENT",
              "executable": {
                "TYPE": "PathExecutable",
                "v1Compatible": true,
                "path": "B.cmd"
              },
              "processLimit": 3,
              "defaultArguments": { "JOB_B": "'B-VALUE'" }}
          },
          "jobResourcePaths": [ "JOB-RESOURCE" ],
          "calendarPath": "CALENDAR",
          "result": {
            "RESULT": "$$RESULT"
          }
        }""")

    "Workflow with positions (for JOC GUI)" in:
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
          "timeZone": "Europe/Berlin",
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
                "processLimit": 3,
                "defaultArguments": { "JOB_A": "'A-VALUE'" }
              }
            }, {
              "position": [ 1 ],
              "label": "TEST-LABEL",
              "TYPE": "If",
              "ifThens": [{
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
                      "processLimit": 3 ,
                      "defaultArguments": { "JOB_B1": "'B1-VALUE'" }
                    }
                  }
                }
              }],
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
                      "processLimit": 3,
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
                          "processLimit": 3,
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
                          "processLimit": 3,
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
                    ],
                    "result": {
                      "RESULT": "$$RESULT"
                    }
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
                "processLimit": 1,
                "subagentBundleIdExpr" : "'SUBAGENT-BUNDLE'"
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
              "processLimit": 3,
              "defaultArguments": { "JOB_A": "'A-VALUE'" }
            },
            "B": {
              "agentPath": "AGENT",
              "executable": {
                "TYPE": "PathExecutable",
                "v1Compatible": true,
                "path": "B.cmd"
              },
              "processLimit": 3,
              "defaultArguments": { "JOB_B": "'B-VALUE'" }}
          },
          "jobResourcePaths": [ "JOB-RESOURCE" ],
          "calendarPath": "CALENDAR",
          "result": {
            "RESULT": "$$RESULT"
          }
        }"""))

    "Workflow with a script" in:
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
              "processLimit": 1
            },
            "OWN-SCRIPT": {
              "agentPath": "AGENT",
              "executable": {
                "TYPE": "ShellScriptExecutable",
                "script": "#!/usr/bin/env bash\n..."
              },
              "processLimit": 1
            }
          }
        }""")

    "Workflow without path" in:
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
                "processLimit": 3,
                "defaultArguments": {
                  "JOB_A": "'A-VALUE'"
                }
              }
            }
          ]
        }""")

    "Single shell script Workflow" in:
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
                "processLimit": 1
              }
            }
          ],
          "path": "WORKFLOW",
          "versionId": "1"
        }""")
  }

  "positionMatchesSearch" in:
    val workflow =
      Workflow(WorkflowPath.NoId,
        Seq(
          "A" @: Execute.Named(WorkflowJob.Name("JOB-A")),
          If(Equal(LastReturnCode, NumericConstant(1))):
            "B" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE")))),
        Map(WorkflowJob.Name("JOB-A") -> WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE")))
      ).completelyChecked.orThrow
    assert(workflow.positionMatchesSearch(Position(0), PositionSearch.ByLabel("A")))
    assert(!workflow.positionMatchesSearch(Position(0), PositionSearch.ByLabel("B")))
    assert(!workflow.positionMatchesSearch(Position(1), PositionSearch.ByLabel("B")))
    assert(!workflow.positionMatchesSearch(Position(99), PositionSearch.ByLabel("A")))
    assert(workflow.positionMatchesSearch(Position(0), PositionSearch.ByWorkflowJob(WorkflowJob.Name("JOB-A"))))
    assert(!workflow.positionMatchesSearch(Position(0), PositionSearch.ByWorkflowJob(WorkflowJob.Name("JOB-X"))))
    assert(workflow.positionMatchesSearch(Position(1) / Then % 0, PositionSearch.ByLabel(Label("B"))))
    assert(!workflow.positionMatchesSearch(Position(1) / Then % 0, PositionSearch.ByWorkflowJob(WorkflowJob.Name("JOB-X"))))

  "labelToPosition of a branch" in:
    val workflow =
      Workflow.of(
        "A" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE"))),
        If(Equal(LastReturnCode, NumericConstant(1))):
          "B" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE")))
      ).completelyChecked.orThrow
    assert(workflow.labelToPosition(Nil, Label("A")) == Right(Position(0)))
    assert(workflow.labelToPosition(Nil, Label("UNKNOWN")) == Left(UnknownKeyProblem("Label", "UNKNOWN")))
    assert(workflow.labelToPosition(Position(1) / Then, Label("B")) == Right(Position(1) / Then % 0))

  "labelToPosition of whole workflow" in:
    val workflow =
      Workflow.of(
        "A" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE"))),
        If(Equal(LastReturnCode, NumericConstant(1))):
          "B" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE")))
      ).completelyChecked.orThrow
    assert(workflow.labelToPosition(Label("A")) == Right(Position(0)))
    assert(workflow.labelToPosition(Label("B")) == Right(Position(1) / Then % 0))
    assert(workflow.labelToPosition(Label("UNKNOWN")) == Left(UnknownKeyProblem("Label", "UNKNOWN")))

  "Duplicate labels" in:
    assert(
      Workflow.of(
        "DUPLICATE" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE"))),
        If(Equal(LastReturnCode, NumericConstant(1))):
          "DUPLICATE" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE")))
      ).completelyChecked ==
        Left(Problem("Label 'DUPLICATE' is duplicated at positions 0, 1/then:0")))

  "Duplicate label in nested workflow" in:
    assert(intercept[RuntimeException] {
      Workflow.of(
        "A" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE"))),
        "A" @: Execute(WorkflowJob(AgentPath("AGENT"), PathExecutable("EXECUTABLE"))))
    }
    .toString.contains("Duplicate labels"))

  "jobOption" in:
    assert(TestWorkflow.checkedExecute(Position(0)) == Right(AExecute))
    assert(TestWorkflow.checkedExecute(Position(1)) == Left(Problem("Expected 'Execute' statement at workflow position 1 (not: If)")))
    assert(TestWorkflow.checkedExecute(Position(2)) == Left(Problem("Expected 'Execute' statement at workflow position 2 (not: Fork)")))
    assert(TestWorkflow.checkedExecute(Position(3)) == Right(lastExecute))
    assert(TestWorkflow.checkedExecute(Position(4)) == Left(Problem("Expected 'Execute' statement at workflow position 4 (not: ImplicitEnd)")))
    assert(TestWorkflow.checkedExecute(Position(999)) == Left(Problem("Expected 'Execute' statement at workflow position 999 (not: Gap)")))

  "workflowOption" in:
    assert(TestWorkflow.nestedWorkflow(Nil) == Right(TestWorkflow))
    assert(TestWorkflow.nestedWorkflow(Position(2) / "fork+🥕") == Right(
      TestWorkflow.instruction(2).asInstanceOf[Fork].workflow(BranchId("fork+🥕")).orThrow))

  "numberedInstruction" in:
    assert(TestWorkflow.numberedInstructions.toSeq == Seq[(InstructionNr, Instruction.Labeled)](
      (InstructionNr(0), AExecute),
      (InstructionNr(1), "TEST-LABEL" @: TestWorkflow.instruction(1)),
      (InstructionNr(2), TestWorkflow.instruction(2)),
      (InstructionNr(3), lastExecute),
      (InstructionNr(4), ImplicitEnd())))

  "flattendWorkflows" in:
    assert(TestWorkflow.flattenedBranchToWorkflow == Map(
      Nil -> TestWorkflow,
      (Position(1) / Then) -> TestWorkflow.instruction(Position(1)).asInstanceOf[If].ifThens.head.thenBlock,
      (Position(1) / Else) -> TestWorkflow.instruction(Position(1)).asInstanceOf[If].elseBlock.get,
      (Position(2) / "fork+🥕") -> TestWorkflow.instruction(Position(2)).asInstanceOf[Fork].branches(0).workflow,
      (Position(2) / "fork+🍋") -> TestWorkflow.instruction(Position(2)).asInstanceOf[Fork].branches(1).workflow,
    ))

  "flattenedInstruction" in:
    assert(TestWorkflow.flattenedInstructions.toSeq == Seq[(Position, Instruction.Labeled)](
      (Position(0), AExecute),
      (Position(1), "TEST-LABEL" @: TestWorkflow.instruction(1)),
      (Position(1) / Then % 0, TestWorkflow.instruction(1).asInstanceOf[If].ifThens.head.thenBlock.instructions(0)),
      (Position(1) / Then % 1, TestWorkflow.instruction(1).asInstanceOf[If].ifThens.head.thenBlock.instructions(1)),
      (Position(1) / Then % 2, ImplicitEnd()),
      (Position(1) / Else % 0, TestWorkflow.instruction(1).asInstanceOf[If].elseBlock.get.instructions(0)),
      (Position(1) / Else % 1, ImplicitEnd()),
      (Position(2), TestWorkflow.instruction(2)),
      (Position(2) / "fork+🥕" % 0, TestWorkflow.instruction(2).asInstanceOf[Fork].branches(0).workflow.instructions(0)),
      (Position(2) / "fork+🥕" % 1, TestWorkflow.instruction(2).asInstanceOf[Fork].branches(0).workflow.instructions(1)),
      (Position(2) / "fork+🥕" % 2, ImplicitEnd()),
      (Position(2) / "fork+🍋" % 0, TestWorkflow.instruction(2).asInstanceOf[Fork].branches(1).workflow.instructions(0)),
      (Position(2) / "fork+🍋" % 1, TestWorkflow.instruction(2).asInstanceOf[Fork].branches(1).workflow.instructions(1)),
      (Position(2) / "fork+🍋" % 2, ImplicitEnd()),
      (Position(3), lastExecute),
      (Position(4), ImplicitEnd())))

  "checkPosition" in:
    assert(TestWorkflow.checkPosition(Position(0)) == Right(()))
    assert(TestWorkflow.checkPosition(Position(1) / Then % 0) == Right(()))
    assert(TestWorkflow.checkPosition(Position(2) / "fork+🥕" % 2) == Right(()))
    assert(TestWorkflow.checkPosition(Position(2) / "fork+🥕" % 3) == Left(Problem("Unknown position 2/fork+🥕:3 in Workflow:TEST~VERSION")))
    assert(TestWorkflow.checkPosition(Position(5)) == Left(Problem("Unknown position 5 in Workflow:TEST~VERSION")))

  "completelyChecked in" - {
    val wrongWorkflow = Workflow(
      WorkflowPath.NoId,
      Vector(
        If(BooleanConstant(true)):
          Execute.Named(AJobName/*undefined*/)))

    "Unknown job" in:
      assert(wrongWorkflow.completelyChecked == Left(Problem("Unknown job name 'A'")))

    "Known job" in:
      val workflow = wrongWorkflow.copy(nameToJob = Map(AJobName -> AJob))
      assert(workflow.completelyChecked == Right(workflow))

    "retry is not allowed outside a catch block" - {
      "simple case" in:
        assert(Workflow.of(Retry()).completelyChecked == Left(Problem("Statement 'retry' is only allowed in a catch block")))
    }

    "in try" in:
      assert(Workflow.of(TryInstruction(Workflow.of(Retry()), Workflow.empty)).completelyChecked
        == Left(Problem("Statement 'retry' is only allowed in a catch block")))

    "in catch" in:
      assert(Workflow.of(TryInstruction(Workflow.empty, Workflow.of(Retry()))).completelyChecked.isRight)

    "'if' in catch" in:
      assert(Workflow.of(TryInstruction(Workflow.empty, Workflow.of(If(BooleanConstant(true))(Retry()))))
        .completelyChecked.isRight)

    "'fork' is a barrier" in:
      assert(Workflow.of(TryInstruction(Workflow.empty, Workflow.of(Fork.of("A" -> Workflow.of(Retry())))))
        .completelyChecked == Left(Problem("Statement 'retry' is only allowed in a catch block")))
  }

  "referencedLockPaths" in:
    val job = WorkflowJob(AgentPath("AGENT"), ShellScriptExecutable(""))
    val a = LockPath("A")
    val b = LockPath("B")
    val c = LockPath("C")
    val workflow = Workflow(
      WorkflowPath("WORKFLOW") ~ "1",
      Vector(
        LockInstruction.single(a, None, Workflow.of(
          Execute(job),
          If(BooleanConstant(true)):
            Workflow.of(
              LockInstruction.single(b, None, Workflow.of(
              Execute(job),
              Fork.of:
                "BRANCH" -> Workflow.of:
                  LockInstruction.single(c, None, Workflow.of:
                    Execute(job)))))))))
    assert(workflow.referencedItemPaths.toSet == Set(a, b, c, AgentPath("AGENT")))

  "referencedBoardPaths" in:
    val a = BoardPath("A")
    val b = BoardPath("B")
    val c = BoardPath("C")
    val workflow = Workflow(
      WorkflowPath("WORKFLOW") ~ "1",
      Vector(
        PostNotices(Seq(a)),
        If(BooleanConstant(true)):
          Fork.of:
            "BRANCH" -> Workflow.of(
              PostNotices(Seq(b)),
              ExpectNotices(BoardPathExpression.ExpectNotice(c)))))
    assert(workflow.referencedItemPaths.toSet == Set(a, b, c))

  "referencedAgentPaths" in:
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
        If(BooleanConstant(true)):
          Workflow.of(
            Execute(bJob),
            Fork.of:
              "BRANCH" -> Workflow.of(
                Execute(cJob)))),
      Map(
        WorkflowJob.Name("D") -> dJob))
    assert(workflow.referencedItemPaths.toSet == Set(a, b, c, d))
    assert(workflow.workflowJobs.toSet == Set(aJob, bJob, cJob, dJob))

  "referencedJobResourcePaths" in:
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
        If(BooleanConstant(true)):
          Workflow.of(
            Execute(job.copy(jobResourcePaths = Seq(b, c))),
            Fork.of:
              "BRANCH" -> Workflow.of(
                Execute(job.copy(jobResourcePaths = Seq(c, d)))))),
      orderPreparation = OrderPreparation(OrderParameterList(
        OrderParameter.Final("V", JobResourceVariable(f, Some("V"))))))
    assert(workflow.referencedItemPaths.toSet == Set(a, b, c, d, e, f, AgentPath("AGENT")))

  "namedJobs" in:
    assert(TestWorkflow.nameToJob == Map(
      AJobName -> AJob,
      BJobName -> BJob))

  "keyToJob" in:
    assert(TestWorkflow.keyToJob == Map(
      JobKey(TestWorkflow.id /: Position(0)) -> AExecute.job,
      JobKey(WorkflowBranchPath(TestWorkflow.id, Position(1) / Then), BJobName) -> B1Job,
      JobKey(TestWorkflow.id /: (Position(1) / Else % 0)) -> BExecute.job,
      JobKey(TestWorkflow.id /: (Position(2) / "fork+🥕" % 0)) -> AExecute.job,
      JobKey(TestWorkflow.id /: (Position(2) / "fork+🍋" % 0)) -> BExecute.job,
      JobKey(TestWorkflow.id /: Position(3)) -> lastExecute.job,
      JobKey(TestWorkflow.id, AJobName) -> AJob,
      JobKey(TestWorkflow.id, BJobName) -> BJob))

  "anonymousJobKey" in:
    val w = Workflow(
      WorkflowPath("TEST") ~ "VERSION",
      Vector(
        If(BooleanConstant(true))     // :0
          .Then(Fail())             // :0/then:0
          .Else(Fail()),            // :0/else:0
        TryInstruction(               // :1
          Workflow.of(Fail()),        // :1/Try:0
          Workflow.of(Fail()))))      // :1/1:0
    assert(w.anonymousJobKey(w.id /: Position(99)) == JobKey.Anonymous(w.id /: Position(99)))
    assert(w.anonymousJobKey(w.id /: (Position(0) / Then % 0)) == JobKey.Anonymous(w.id /: (Position(0) / Then % 0)))
    assert(w.anonymousJobKey(w.id /: (Position(0) / Else % 0)) == JobKey.Anonymous(w.id /: (Position(0) / Else % 0)))
    assert(w.anonymousJobKey(w.id /: (Position(1) / try_(0) % 0)) == JobKey.Anonymous(w.id /: (Position(1) / Try_ % 0)))
    // anonymousJobKey normalizes the retry-index of a Retry Position to 0.
    assert(w.anonymousJobKey(w.id /: (Position(1) / try_(1)    % 0)) == JobKey.Anonymous(w.id /: (Position(1) / Try_ % 0)))

  "isDefinedAt, instruction" in:
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
      Position(3) -> lastExecute,
      Position(4) -> ImplicitEnd())

    for (address, instruction) <- addressToInstruction do
      assert(TestWorkflow.isDefinedAt(address))
      assert(TestWorkflow.instruction(address) == instruction, s" - $address")
    assert(!TestWorkflow.isDefinedAt(Position(0) / "fork+🥕" % 0))
    assert(!TestWorkflow.isDefinedAt(Position(0) / "fork+🥕" % 3))
    assert(!TestWorkflow.isDefinedAt(Position(999)))

  "isMoveable, reachablePositions" - {
    "Same BranchPath is okay" in:
      for i <- TestWorkflow.instructions.indices; j <- TestWorkflow.instructions.indices do
        assert(TestWorkflow.isMoveable(Position(i), Position(j)))

    "Undefined Position is not okay" in:
      assert(!TestWorkflow.isMoveable(Position(0), Position(TestWorkflow.instructions.length)))
      assert(!TestWorkflow.isMoveable(Position(TestWorkflow.instructions.length), Position(0)))

    "if-then-else is okay" in:
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

    "if-then-else-if-... is okay" in:
      val workflow = Workflow.of(WorkflowPath("WORKFLOW") ~ "1.0",
        If(expr("false")).Then:
          Workflow.empty
        .elseIf(expr("false")).Then:
          Workflow.empty
        .elseIf(expr("true")).Then:
          Workflow.empty
        .Else:
          Workflow.empty)

      assert:
        workflow.reachablePositions(Position(0)).toList ==
          List(
            Position(0),
            Position(0) / "then" % 0,
            Position(0) / "then+2" % 0,
            Position(0) / "then+3" % 0,
            Position(0) / "else" % 0,
            Position(1))

    "Different fork branches are not okay" in:
      assert(!TestWorkflow.isMoveable(Position(2), Position(2) / fork("🥕") % 0))
      assert(!TestWorkflow.isMoveable(Position(2) / fork("🥕") % 0, Position(2) / fork("🍋") % 0))

    "Same fork branch is okay" in:
      assert(TestWorkflow.isMoveable(Position(2) / fork("🥕") % 0, Position(2) / fork("🥕") % 0))
      assert(TestWorkflow.isMoveable(Position(2) / fork("🥕") % 0, Position(2) / fork("🥕") % 2))

    "Lock" - {
      lazy val lockWorkflow = Workflow(WorkflowPath("TEST") ~ "1",
        Vector(
          AExecute,
          LockInstruction.single(LockPath("LOCK"), None, Workflow.of:
            If(BooleanConstant(true)):
              AExecute)),
        Map(
          AJobName -> AJob))

      "Into same locked block" in:
        assert(lockWorkflow.isMoveable(Position(1) / BranchId.Lock % 0, Position(1) / BranchId.Lock % 0))
        assert(lockWorkflow.isMoveable(Position(1) / BranchId.Lock % 0, Position(1) / BranchId.Lock % 0 / Then % 0))
        assert(lockWorkflow.isMoveable(Position(1) / BranchId.Lock % 0 / Then % 0, Position(1) / BranchId.Lock % 0))

      "Into a locked block is not okay" in:
        assert(!lockWorkflow.isMoveable(Position(1), Position(1) / BranchId.Lock % 0))

      "Out of a locked block is not okay" in:
        assert(!lockWorkflow.isMoveable(Position(1) / BranchId.Lock % 0, Position(1)))

      "reachablePositions" in:
        val reachable = Seq(
          Position(1) / BranchId.Lock % 0,
          Position(1) / BranchId.Lock % 0 / Then % 0,
          Position(1) / BranchId.Lock % 0 / Then % 1,
          Position(1) / BranchId.Lock % 1)

        for from <- reachable do
          assert(lockWorkflow.reachablePositions(from).toSeq == reachable)
    }

    "try catch" - {
      lazy val tryWorkflow =
        val execute = Execute.Anonymous(WorkflowJob(AgentPath("AGENT"), PathExecutable("SCRIPT")))
        Workflow.of(
          execute,
          TryInstruction(
            tryWorkflow = Workflow.of(execute),
            catchWorkflow = Workflow.of(execute)),
          execute)

      "try catch is okay" in:
        assert(tryWorkflow.isMoveable(Position(1), Position(1) / Try_ % 0))
        assert(tryWorkflow.isMoveable(Position(1), Position(1) / Catch_ % 0))
        assert(tryWorkflow.isMoveable(Position(1) / Catch_ % 0, Position(1)))

      "reachablePositions" in:
        val reachable = Seq(
          Position(0),
          Position(1),
          Position(1) / Try_ % 0,
          Position(1) / Try_ % 1,
          Position(1) / Catch_ % 0,
          Position(1) / Catch_ % 1,
          Position(2),
          Position(3))

        for from <- reachable do
          assert(tryWorkflow.reachablePositions(from).toSeq == reachable)
    }

    "reachablePositions from first level" in:
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

      for from <- reachable do
        assert(TestWorkflow.reachablePositions(from).toSeq == reachable)

    "reachablePositions from fork" in:
      val reachable = Seq(
        Position(2) / fork("🥕") % 0,
        Position(2) / fork("🥕") % 1,
        Position(2) / fork("🥕") % 2)

      for from <- reachable do
        assert(TestWorkflow.reachablePositions(from).toSeq == reachable)

    // For reachablePositions for Lock and Try, see above.
  }

  "Workflow with a Lock and a Job" in:
    Workflow(WorkflowPath("WORKFLOW"),
        Vector(
          LockInstruction.single(
            LockPath("LOCK"), count = None, Workflow.of(
              Execute.Named(WorkflowJob.Name("JOB"))))),
        Map(
          WorkflowJob.Name("JOB") -> AJob))
      .completelyChecked
      .orThrow: Workflow

  "reduceForAgent" - {
    import js7.data.workflow.test.ForkTestSetting.*

    "reduceForAgent A" in:
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

    "reduceForAgent B" in:
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

    "reduceForAgent with LockInstruction" in:
      val workflow = Workflow(WorkflowPath.Anonymous,
        Vector(
          LockInstruction.single(LockPath("LOCK"), count=None, Workflow.of(
            AExecute,
            Fork.of(
              "🥕" -> Workflow.of(AExecute))))),
        Map(AJobName -> AJob))
      assert(workflow.reduceForAgent(AAgentPath) eq workflow)
      assert(workflow.reduceForAgent(BAgentPath) == Workflow.of(Gap()))

    "reduceForAgent with LockInstruction and more (1)" in:
      val workflow = Workflow(WorkflowPath.Anonymous,
        Vector(
          LockInstruction.single(LockPath("LOCK"), count=None, Workflow.of(
            If(BooleanConstant(true)):
              Workflow.of(
                TryInstruction(Workflow.empty, Workflow.empty),
                Fail()),
            AExecute,
            Fork.of(
              "🥕" -> Workflow.of(AExecute))))),
        Map(AJobName -> AJob))
      assert(workflow.reduceForAgent(AAgentPath) eq workflow)
      assert(workflow.reduceForAgent(BAgentPath) == Workflow.of(Gap()))

    "reduceForAgent with LockInstruction and if" in:
      val workflow = Workflow(WorkflowPath.Anonymous,
        Vector(
          LockInstruction.single(LockPath("LOCK"), count=None, Workflow.of(
            AExecute,
            If(BooleanConstant(true)).Then:
              Workflow.of(
                BExecute,
                TryInstruction(Workflow.empty, Workflow.empty),
                Fail())
            .Else:
              BExecute,
            AExecute))),
        Map(
          AJobName -> AJob,
          BJobName -> BJob))

      assert(workflow.reduceForAgent(AAgentPath) ==
        Workflow(WorkflowPath.Anonymous,
          Vector(
            LockInstruction.single(LockPath("LOCK"), count=None, Workflow.of(
              AExecute,
              If(BooleanConstant(true)).Then:
                Workflow.of(
                  Gap(),
                  TryInstruction(Workflow.empty, Workflow.empty),
                  Fail())
              .Else:
                Gap(),
              AExecute))),
          Map(
            AJobName -> AJob)))

      assert(workflow.reduceForAgent(BAgentPath) ==
        Workflow(WorkflowPath.Anonymous,
          Vector(
            LockInstruction.single(LockPath("LOCK"), count=None, Workflow.of(
              Gap(),
              If(BooleanConstant(true)).Then:
                Workflow.of(
                  BExecute,
                  TryInstruction(Workflow.empty, Workflow.empty),
                  Fail())
              .Else:
                BExecute,
              Gap()))),
          Map(
            BJobName -> BJob)))

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

      for (position, agentPaths) <- isStartableSetting do
        for (agentPath, expected) <- agentPaths.map(_ -> true) ++ (AgentPaths filterNot agentPaths.toSet).map(_ -> false) do
          s"isStartableOnAgent($position $agentPath) = $expected" in:
            assert(TestWorkflow.isStartableOnAgent(position, agentPath) == expected)
          s".reduceForAgent.isStartableOnAgent($position $agentPath) = $expected" in:
            //assert(SimpleTestWorkflow.reduceForAgent(agentPath).isStartableOnAgent(position, agentPath))
            assert(TestWorkflow.reduceForAgent(agentPath).isStartableOnAgent(position, agentPath) == expected)
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

  "knownSubagentBundleIds" in:
    assert(TestWorkflow.knownSubagentBundleIds == Set(SubagentBundleId("SUBAGENT-BUNDLE")))

  "referencedItemPaths" in:
    assert(TestWorkflow.referencedItemPaths.toSet == Set(
      TestAgentPath,
      JobResourcePath("JOB-RESOURCE"),
      CalendarPath("CALENDAR"),
      SubagentBundleId("SUBAGENT-BUNDLE")))

    assert(ForkTestSetting.TestWorkflow.referencedItemPaths.toSet == Set(
      ForkTestSetting.AAgentPath,
      ForkTestSetting.BAgentPath))

private object WorkflowTest:

  private val lastExecute = Execute(WorkflowJob(TestAgentPath, BPathExecutable, subagentBundleId =
    Some(expr("'SUBAGENT-BUNDLE'"))))

  private val TestWorkflow = Workflow(
    WorkflowPath("TEST") ~ "VERSION",
    Vector(
      AExecute,
      "TEST-LABEL" @:
        If(Equal(LastReturnCode, NumericConstant(1))).Then:
          Workflow.anonymous(
            Vector(
              Execute.Named(AJobName),
              Execute.Named(BJobName)),
            Map(
              BJobName -> B1Job))
        .Else:
          BExecute,
      Fork.of(
        "🥕" -> Workflow.of(
          AExecute,
          Execute.Named(AJobName)),
        "🍋" -> Workflow.anonymous(
          Seq(
            BExecute,
            Execute.Named(BJobName)),
          result = Some(Map(
            "RESULT" -> expr("$RESULT"))))),
      lastExecute),
    Map(
      AJobName -> AJob,
      BJobName -> BJob),
    OrderPreparation(
      OrderParameterList(
        Seq(
          OrderParameter("myRequired", NumberValue),
          OrderParameter("myOptional", StringConstant("DEFAULT")),
          OrderParameter.Final("myFinal", StringConstant("FINAL"))),
        allowUndeclared = true)),
    Timezone("Europe/Berlin"),
    jobResourcePaths = Seq(
      JobResourcePath("JOB-RESOURCE")),
    calendarPath = Some(CalendarPath("CALENDAR")),
    result = Some(Map(
      "RESULT" -> expr("$RESULT"))))
